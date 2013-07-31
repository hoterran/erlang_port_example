# summary

Erlang use open_port interact with  java and python program

http://www.kazmier.net/computer/port-howto/

# use

cd erlang_port_example/lib/echo_app-1.0/

erlc -W -o ebin src/*.erl

%absolute path
erl -pa ~/erlang_port_example/lib/echo_app-1.0/ebin

		application:start(echo_app).

%python test

		echo:echopy("Testing\n").

%java test

		L = "select 1". 

		echo:echojava(<<(byte_size(list_to_binary(L))):24/little, (list_to_binary(L))/binary>>).
