all:
	make compile_web_app

run_tests:
	elm-test

compile_web_app:
	elm make src/Main.elm --output=main.js