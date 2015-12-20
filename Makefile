all: run

%.beam: %.erl
	erlc $<

run: ttt.beam
	erl -noshell -s ttt start_game -s init stop

