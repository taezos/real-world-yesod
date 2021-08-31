ghcid-lib:
	ghcid -c "stack repl real-world-yesod:lib"

ghcid-test:
	ghcid -c "stack repl real-world-yesod:real-world-yesod-test"

ghcid-lib-test:
	ghcid -c "stack repl real-world-yesod:lib real-world-yesod:real-world-yesod-test" --test="main"
