.DEFAULT_GOAL := postgres

.PHONY: postgres, postgres-down

postgres:
	 docker-compose --file Docker/docker-compose.yml up --build

postgres-down:
	 docker-compose --file Docker/docker-compose.yml down --remove-orphans
