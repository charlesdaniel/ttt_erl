# We use the tiny Erlang/OTP on Alpine Linux as the base
# https://hub.docker.com/r/msaraiva/erlang/
FROM msaraiva/erlang

MAINTAINER Charles Daniel <charles.sam.daniel@gmail.com>

WORKDIR /home/ttt
ADD . /home/ttt

# Compile the Erlang program to a beam file
RUN erlc ttt.erl

# Default command that is run on a docker run
CMD erl -noshell -s ttt start_game -s init stop
