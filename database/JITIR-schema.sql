-- JITIR Database Schema for Guile Multilanguage Examples
-- Just-In-Time Information Retrieval System
-- Version: 1.0.0
-- Platform: FreeBSD 14.3 / PostgreSQL 15+

-- ============================================
-- Core Domain: Language Processing & Compilation
-- ============================================

CREATE SCHEMA IF NOT EXISTS multilang;
CREATE SCHEMA IF NOT EXISTS demos;
CREATE SCHEMA IF NOT EXISTS development;
CREATE SCHEMA IF NOT EXISTS jitir;

-- ============================================
-- JITIR Metadata Tables
-- ============================================

CREATE TABLE jitir.retrieval_contexts (
    context_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    context_name VARCHAR(255) NOT NULL,
    description TEXT,
    priority INTEGER DEFAULT 50,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE jitir.information_nodes (
    node_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    context_id UUID REFERENCES jitir.retrieval_contexts(context_id),
    node_type VARCHAR(50) NOT NULL, -- 'code', 'demo', 'config', 'documentation'
    payload JSONB NOT NULL,
    retrieval_score FLOAT DEFAULT 0.0,
    access_count INTEGER DEFAULT 0,
    last_accessed TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- ============================================
-- Language Support & Compilation
-- ============================================

CREATE TABLE multilang.supported_languages (
    language_id SERIAL PRIMARY KEY,
    language_name VARCHAR(50) UNIQUE NOT NULL,
    language_code VARCHAR(20) UNIQUE NOT NULL,
    from_syntax VARCHAR(50), -- 'scheme', 'elisp', 'brainfuck', 'ecmascript'
    to_format VARCHAR(50), -- 'value', 'tree-il', 'bytecode'
    status VARCHAR(20) DEFAULT 'active', -- 'active', 'experimental', 'broken'
    guile_version_min VARCHAR(20),
    guile_version_max VARCHAR(20),
    implementation_path TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB DEFAULT '{}'::jsonb
);

CREATE TABLE multilang.compilation_pipelines (
    pipeline_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    source_language_id INTEGER REFERENCES multilang.supported_languages(language_id),
    target_format VARCHAR(50) NOT NULL,
    compilation_stages JSONB NOT NULL, -- Array of transformation steps
    performance_metrics JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE multilang.compilation_history (
    compilation_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    pipeline_id UUID REFERENCES multilang.compilation_pipelines(pipeline_id),
    source_code TEXT NOT NULL,
    source_hash VARCHAR(64) GENERATED ALWAYS AS (encode(sha256(source_code::bytea), 'hex')) STORED,
    compiled_output TEXT,
    tree_il_representation TEXT,
    bytecode BYTEA,
    execution_result JSONB,
    compilation_time_ms INTEGER,
    success BOOLEAN DEFAULT false,
    error_details JSONB,
    environment JSONB, -- Guile version, OS, etc.
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- ============================================
-- Demo Management System
-- ============================================

CREATE TABLE demos.recordings (
    recording_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    recording_type VARCHAR(50) NOT NULL, -- 'asciinema', 'gif', 'video', 'screenshot'
    title VARCHAR(255) NOT NULL,
    description TEXT,
    file_path TEXT NOT NULL,
    file_size_bytes BIGINT,
    duration_seconds FLOAT,
    format VARCHAR(50),
    resolution VARCHAR(20),
    metadata JSONB DEFAULT '{}'::jsonb,
    tags TEXT[],
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE demos.demo_sessions (
    session_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_name VARCHAR(255) NOT NULL,
    tmux_session_name VARCHAR(100),
    start_time TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    end_time TIMESTAMP WITH TIME ZONE,
    status VARCHAR(50) DEFAULT 'active', -- 'active', 'completed', 'failed'
    environment JSONB,
    commands_executed TEXT[],
    recording_ids UUID[],
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE demos.instant_captures (
    capture_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID REFERENCES demos.demo_sessions(session_id),
    trigger_key VARCHAR(50), -- 'option', 'alt+r', etc.
    buffer_duration_seconds INTEGER DEFAULT 30,
    capture_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    recording_id UUID REFERENCES demos.recordings(recording_id),
    auto_converted_to_gif BOOLEAN DEFAULT false
);

-- ============================================
-- Development Environment Configuration
-- ============================================

CREATE TABLE development.emacs_config (
    config_id SERIAL PRIMARY KEY,
    config_name VARCHAR(100) UNIQUE NOT NULL,
    init_file_content TEXT,
    packages JSONB, -- List of packages and versions
    key_bindings JSONB,
    integrations JSONB, -- Geiser, Magit, Forge, Claude Code
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    active BOOLEAN DEFAULT true
);

CREATE TABLE development.git_submodules (
    submodule_id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    path TEXT NOT NULL,
    url TEXT NOT NULL,
    branch VARCHAR(100),
    commit_hash VARCHAR(40),
    category VARCHAR(50), -- 'core', 'tools', 'debugging', 'documentation'
    description TEXT,
    last_updated TIMESTAMP WITH TIME ZONE,
    metadata JSONB DEFAULT '{}'::jsonb
);

CREATE TABLE development.build_configurations (
    build_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    build_name VARCHAR(100) NOT NULL,
    makefile_target VARCHAR(100),
    command TEXT NOT NULL,
    dependencies TEXT[],
    environment_vars JSONB,
    success_count INTEGER DEFAULT 0,
    failure_count INTEGER DEFAULT 0,
    average_duration_ms INTEGER,
    last_run TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- ============================================
-- Testing & Validation
-- ============================================

CREATE TABLE development.test_suites (
    suite_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    suite_name VARCHAR(100) NOT NULL,
    language_id INTEGER REFERENCES multilang.supported_languages(language_id),
    test_files TEXT[],
    expected_results JSONB,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE development.test_runs (
    run_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    suite_id UUID REFERENCES development.test_suites(suite_id),
    run_timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    guile_version VARCHAR(20),
    os_version VARCHAR(100),
    passed INTEGER DEFAULT 0,
    failed INTEGER DEFAULT 0,
    skipped INTEGER DEFAULT 0,
    duration_ms INTEGER,
    detailed_results JSONB,
    artifacts_path TEXT
);

-- ============================================
-- Documentation & Presentation
-- ============================================

CREATE TABLE development.documentation (
    doc_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    doc_type VARCHAR(50) NOT NULL, -- 'readme', 'guide', 'presentation', 'report'
    title VARCHAR(255) NOT NULL,
    file_path TEXT NOT NULL,
    content_hash VARCHAR(64),
    format VARCHAR(20), -- 'markdown', 'pdf', 'html'
    version VARCHAR(20),
    tags TEXT[],
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- ============================================
-- JITIR Indexing for Fast Retrieval
-- ============================================

CREATE INDEX idx_jitir_nodes_context ON jitir.information_nodes(context_id);
CREATE INDEX idx_jitir_nodes_type ON jitir.information_nodes(node_type);
CREATE INDEX idx_jitir_nodes_score ON jitir.information_nodes(retrieval_score DESC);
CREATE INDEX idx_jitir_nodes_payload ON jitir.information_nodes USING GIN(payload);

CREATE INDEX idx_compilation_source_hash ON multilang.compilation_history(source_hash);
CREATE INDEX idx_compilation_success ON multilang.compilation_history(success);
CREATE INDEX idx_compilation_created ON multilang.compilation_history(created_at DESC);

CREATE INDEX idx_recordings_type ON demos.recordings(recording_type);
CREATE INDEX idx_recordings_tags ON demos.recordings USING GIN(tags);
CREATE INDEX idx_recordings_created ON demos.recordings(created_at DESC);

CREATE INDEX idx_test_runs_suite ON development.test_runs(suite_id);
CREATE INDEX idx_test_runs_timestamp ON development.test_runs(run_timestamp DESC);

-- ============================================
-- Views for JITIR Quick Access
-- ============================================

CREATE VIEW jitir.active_demos AS
SELECT 
    r.recording_id,
    r.title,
    r.recording_type,
    r.file_path,
    r.duration_seconds,
    r.created_at,
    array_agg(DISTINCT t.tag) as all_tags
FROM demos.recordings r
LEFT JOIN LATERAL unnest(r.tags) as t(tag) ON true
WHERE r.created_at > CURRENT_DATE - INTERVAL '30 days'
GROUP BY r.recording_id;

CREATE VIEW jitir.language_status AS
SELECT 
    sl.language_name,
    sl.language_code,
    sl.status,
    COUNT(ch.compilation_id) as total_compilations,
    SUM(CASE WHEN ch.success THEN 1 ELSE 0 END) as successful_compilations,
    AVG(ch.compilation_time_ms) as avg_compilation_time_ms,
    MAX(ch.created_at) as last_used
FROM multilang.supported_languages sl
LEFT JOIN multilang.compilation_pipelines cp ON sl.language_id = cp.source_language_id
LEFT JOIN multilang.compilation_history ch ON cp.pipeline_id = ch.pipeline_id
GROUP BY sl.language_id;

CREATE VIEW jitir.recent_captures AS
SELECT 
    ic.capture_id,
    ic.trigger_key,
    ic.capture_timestamp,
    ds.session_name,
    r.file_path,
    r.format
FROM demos.instant_captures ic
JOIN demos.demo_sessions ds ON ic.session_id = ds.session_id
LEFT JOIN demos.recordings r ON ic.recording_id = r.recording_id
WHERE ic.capture_timestamp > CURRENT_TIMESTAMP - INTERVAL '24 hours'
ORDER BY ic.capture_timestamp DESC;

-- ============================================
-- Stored Procedures for JITIR Operations
-- ============================================

CREATE OR REPLACE FUNCTION jitir.record_compilation(
    p_source_code TEXT,
    p_from_language VARCHAR,
    p_to_format VARCHAR,
    p_result JSONB
) RETURNS UUID AS $$
DECLARE
    v_language_id INTEGER;
    v_pipeline_id UUID;
    v_compilation_id UUID;
BEGIN
    -- Find language
    SELECT language_id INTO v_language_id
    FROM multilang.supported_languages
    WHERE language_code = p_from_language;
    
    -- Find or create pipeline
    SELECT pipeline_id INTO v_pipeline_id
    FROM multilang.compilation_pipelines
    WHERE source_language_id = v_language_id
    AND target_format = p_to_format
    LIMIT 1;
    
    IF v_pipeline_id IS NULL THEN
        INSERT INTO multilang.compilation_pipelines (source_language_id, target_format, compilation_stages)
        VALUES (v_language_id, p_to_format, '[]'::jsonb)
        RETURNING pipeline_id INTO v_pipeline_id;
    END IF;
    
    -- Record compilation
    INSERT INTO multilang.compilation_history (
        pipeline_id, source_code, execution_result, success
    ) VALUES (
        v_pipeline_id, p_source_code, p_result, true
    ) RETURNING compilation_id INTO v_compilation_id;
    
    -- Update JITIR node
    INSERT INTO jitir.information_nodes (
        node_type, payload, retrieval_score
    ) VALUES (
        'compilation', 
        jsonb_build_object(
            'compilation_id', v_compilation_id,
            'source', p_source_code,
            'result', p_result
        ),
        1.0
    );
    
    RETURN v_compilation_id;
END;
$$ LANGUAGE plpgsql;

-- ============================================
-- Initial Data Population
-- ============================================

INSERT INTO multilang.supported_languages (language_name, language_code, from_syntax, status, guile_version_min) VALUES
('GNU Scheme', 'scheme', 'scheme', 'active', '2.2'),
('Emacs Lisp', 'elisp', 'elisp', 'active', '2.2'),
('Brainfuck', 'brainfuck', 'brainfuck', 'active', '2.2'),
('ECMAScript', 'ecmascript', 'ecmascript', 'broken', '3.0');

INSERT INTO jitir.retrieval_contexts (context_name, description, priority) VALUES
('Demo System', 'Instant recording and demo capture system', 100),
('Compilation Pipeline', 'Multilanguage compilation and execution', 90),
('Development Environment', 'Emacs, Geiser, and IDE integrations', 80),
('Testing Framework', 'Test suites and validation', 70);

-- ============================================
-- Comments & Documentation
-- ============================================

COMMENT ON SCHEMA jitir IS 'Just-In-Time Information Retrieval system for rapid access to project data';
COMMENT ON TABLE multilang.compilation_history IS 'Complete history of all compilation attempts with source, output, and metrics';
COMMENT ON TABLE demos.instant_captures IS 'Ring-buffer recordings triggered by hotkeys for spontaneous demo capture';
COMMENT ON TABLE development.emacs_config IS 'Emacs configuration including packages, keybindings, and integrations';
COMMENT ON FUNCTION jitir.record_compilation IS 'Records a compilation event and updates JITIR nodes for fast retrieval';