# GitHub CLI Invitation Management

## Viewing Invitations

### Repository Invitations
Check pending repository collaboration invitations:
```bash
# List all pending repository invitations
gh api /user/repository_invitations

# Show repository names only
gh api /user/repository_invitations --jq '.[].repository.full_name'

# Show detailed invitation info
gh api /user/repository_invitations --jq '.[] | {
  repo: .repository.full_name,
  id: .id,
  inviter: .inviter.login,
  permissions: .permissions
}'
```

### Organization Invitations
Check organization membership invitations:
```bash
# List organization memberships (including pending)
gh api /user/memberships/orgs

# Filter pending invitations
gh api /user/memberships/orgs --jq '.[] | select(.state == "pending")'
```

## Managing Invitations

### Accept Repository Invitation
```bash
# Accept by invitation ID
gh api --method PATCH /user/repository_invitations/{invitation_id}

# Example
gh api --method PATCH /user/repository_invitations/12345678
```

### Decline Repository Invitation
```bash
# Decline by invitation ID
gh api --method DELETE /user/repository_invitations/{invitation_id}

# Example
gh api --method DELETE /user/repository_invitations/12345678
```

### Accept Organization Invitation
```bash
# Accept organization membership
gh api --method PATCH /user/memberships/orgs/{org} \
  -f state=active

# Example
gh api --method PATCH /user/memberships/orgs/my-org \
  -f state=active
```

## Sending Invitations

### Invite Collaborator to Repository
```bash
# Invite a user to collaborate
gh api repos/{owner}/{repo}/collaborators/{username} \
  --method PUT \
  -f permission=push

# Example
gh api repos/dsp-dr/guile-multilang-examples/collaborators/someuser \
  --method PUT \
  -f permission=push
```

### Check Pending Invitations You've Sent
```bash
# List invitations for a repository you own
gh api repos/{owner}/{repo}/invitations

# Example for this repo
gh api repos/dsp-dr/guile-multilang-examples/invitations
```

## Useful Aliases

Add these to your shell configuration:
```bash
# Check all invitations
alias gh-invites='gh api /user/repository_invitations --jq ".[].repository.full_name"'

# Accept invitation by repo name (requires jq)
gh-accept() {
  local repo="$1"
  local id=$(gh api /user/repository_invitations --jq ".[] | select(.repository.full_name == \"$repo\") | .id")
  if [ -n "$id" ]; then
    gh api --method PATCH /user/repository_invitations/$id
    echo "Accepted invitation to $repo"
  else
    echo "No invitation found for $repo"
  fi
}
```

## Notes
- Invitations expire after 7 days if not accepted
- Repository invitations require the appropriate OAuth scopes
- Organization invitations may require 2FA depending on org settings
- Use `--jq` flag for JSON parsing within gh CLI
- Use `--paginate` for repositories with many invitations