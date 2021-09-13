ghcid-lib:
	ghcid -c "stack repl real-world-yesod:lib"

ghcid-unit-test:
	ghcid -c "stack repl real-world-yesod:unit"

ghcid-integration-test:
	ghcid -c "stack repl real-world-yesod:integration"

ghcid-lib-unit-test:
	ghcid -c "stack repl real-world-yesod:lib real-world-yesod:unit" --test="main"

ghcid-lib-integration-test:
	ghcid -c "stack repl real-world-yesod:lib real-world-yesod:integration" --test="main"
