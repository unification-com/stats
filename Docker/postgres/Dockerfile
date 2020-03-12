FROM postgres:12.2-alpine

WORKDIR /app

USER postgres

COPY Docker/postgres/data/ /docker-entrypoint-initdb.d/

RUN echo "alias ll='ls -la'" >> /var/lib/postgresql/.bashrc

EXPOSE 5432
CMD ["postgres"]
