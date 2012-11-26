compile:
	./rebar compile

clean:
	./rebar clean

run:
	@exec erl -pa ebin -s traffic_control start


test:
	./rebar eunit
