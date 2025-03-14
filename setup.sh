#!/bin/bash
# Process Code files - move up three levels and rename
for ext in cpp; do
	find . -name "*.$ext" -type f | while read -r file; do
		dir=$(basename "$(dirname "$file")")
		up=$(dirname "$(dirname "$(dirname "$(dirname "$file")")")")
		mv -n "$file" "$up/$dir.erl"
	done
done

echo "Setup completed successfully!"
