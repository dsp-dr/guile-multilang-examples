#!/bin/bash
# Script to accept all pending GitHub repository invitations

echo "=== GitHub Repository Invitations Manager ==="
echo ""

# Get all invitations
INVITATIONS=$(gh api /user/repository_invitations)
COUNT=$(echo "$INVITATIONS" | jq 'length')

if [ "$COUNT" -eq 0 ]; then
    echo "No pending invitations found."
    exit 0
fi

echo "Found $COUNT pending invitation(s):"
echo ""

# List all invitations
echo "$INVITATIONS" | jq -r '.[] | "  • \(.repository.full_name) (from \(.inviter.login))"'
echo ""

# Ask for confirmation
read -p "Do you want to accept all invitations? (y/n/select): " response

case $response in
    [Yy]* )
        echo ""
        echo "Accepting all invitations..."
        echo "$INVITATIONS" | jq -r '.[].id' | while read -r id; do
            repo=$(echo "$INVITATIONS" | jq -r ".[] | select(.id == $id) | .repository.full_name")
            echo -n "  Accepting $repo... "
            if gh api --method PATCH /user/repository_invitations/$id >/dev/null 2>&1; then
                echo "✓"
            else
                echo "✗ (failed)"
            fi
        done
        echo ""
        echo "Done! All invitations processed."
        ;;
    
    [Nn]* )
        echo "No invitations accepted."
        ;;
    
    [Ss]* )
        echo ""
        echo "Select invitations to accept:"
        echo "$INVITATIONS" | jq -r '.[] | "\(.id) \(.repository.full_name)"' | while read -r id repo; do
            read -p "  Accept $repo? (y/n): " accept
            if [[ $accept == [Yy]* ]]; then
                echo -n "    Accepting... "
                if gh api --method PATCH /user/repository_invitations/$id >/dev/null 2>&1; then
                    echo "✓"
                else
                    echo "✗ (failed)"
                fi
            fi
        done
        ;;
    
    * )
        echo "Invalid response. Exiting."
        ;;
esac

# Show remaining invitations
echo ""
REMAINING=$(gh api /user/repository_invitations | jq 'length')
echo "Remaining invitations: $REMAINING"