all: run

%.beam: %.erl
	erlc $<

run: ttt.beam
	erl -noshell -s ttt start_game -s init stop

docker-run:
	@echo "\nBuilding Docker image...\n"
	docker build -t ttt .
	@echo "\nDocker Build Complete... Now Running ttt within docker\n\n\n"
	docker run --rm -it ttt
