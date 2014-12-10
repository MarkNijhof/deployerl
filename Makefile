PROJECT = deployerl

ERLC_OPTS = +debug_info +'{parse_transform,lager_transform}'

DEPS = lager jiffy exometer

dep_jiffy    = git https://github.com/davisp/jiffy master
dep_exometer = git https://github.com/MarkNijhof/exometer master

include erlang.mk

run:
	./run.sh
run_server:
	DEPLOYERL_MODE=server ./run.sh
run_client:
	DEPLOYERL_MODE=client ./run.sh

start:
	./start.sh
start_server:
	DEPLOYERL_MODE=server ./start.sh
start_client:
	DEPLOYERL_MODE=client ./start.sh

stop:
	./stop.sh






CONTAINER_NAME=deployerl

docker.build:
	cd docker && docker build --rm -t ${CONTAINER_NAME} .

docker.shell.server:
	cd docker && docker run --rm -ti -e DEPLOYERL_MODE=server -h "deployerl_server_`cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 8 | head -n 1`" -v $(shell pwd):/root/src ${CONTAINER_NAME} /sbin/my_init -- bash -l

docker.shell.client:
	cd docker && docker run --rm -ti -e DEPLOYERL_MODE=client -h "deployerl_client_`cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 8 | head -n 1`" -v $(shell pwd):/root/src ${CONTAINER_NAME} /sbin/my_init -- bash -l

docker.clean: docker.clean.containers docker.clean.none-images
	@echo "Clean all"
	docker rmi ${CONTAINER_NAME}:latest

docker.clean.containers:
	@echo "Clean stopped containers"
	docker rm `docker ps --no-trunc -a -q`

docker.clean.none-images:
	@echo "Clean <none> images"
	docker rmi `docker images | grep "^<none>" | awk "{print $3}"`
