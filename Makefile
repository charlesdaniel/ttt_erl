all: run

%.beam: %.erl
	erlc $<

run: ttt.beam
	erl -noshell -s ttt start_game -s init stop

docker-build-beam:
	docker run --rm -it -v $(shell pwd):/home/ttt -w /home/ttt msaraiva/erlang erlc ttt.erl

docker-run: docker-build-beam
	docker run --rm -it -v $(shell pwd):/home/ttt -w /home/ttt msaraiva/erlang erl -noshell -s ttt start_game -s init stop
