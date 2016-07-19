# We use the tiny Erlang/OTP on Alpine Linux as the base
# https://hub.docker.com/r/msaraiva/erlang/
FROM msaraiva/erlang

MAINTAINER Charles Daniel <charles.sam.daniel@gmail.com>

# Create a new user called ttt_user and run it as that.
# This is so we don't go running things as root.
RUN adduser -h /home/ttt -D ttt_user
USER ttt_user
WORKDIR /home/ttt
ADD . /home/ttt

# Compile the Erlang program to a beam file
RUN erlc ttt.erl

# Default command that is run on a docker run
CMD erl -noshell -s ttt start_game -s init stop
