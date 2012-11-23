compile:
	./rebar compile

clean:
	./rebar clean

run:
	@exec erl -pa ebin -s sr_simulate start


