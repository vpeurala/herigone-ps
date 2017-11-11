#!/usr/bin/env bash
docker container stop herigone-ps-db 2>/dev/null;
docker container rm herigone-ps-db 2>/dev/null;
docker container create --interactive --tty --env POSTGRES_PASSWORD='a7vc0Foqv4KrqCmL' --name=herigone-ps-db --network=herigone-network --user=postgres --publish 5432:5432 vpeurala/herigone-ps-db:latest;
docker container start herigone-ps-db;

echo "Waiting for the database to accept connections...";
while true; do
  pg_isready --dbname=herigone --host=localhost --port=5432 --quiet --username=postgres;
  case "$?" in
    0) # server is accepting connections normally
      echo "The database is ready to accept connections.";
      exit 0;
      ;;
    1) # server is rejecting connections
      continue;
      ;;
    2) # there was no response to the connection attempt
      continue;
      ;;
    3) # no attempt was made (for example due to invalid parameters)
      echo "Invalid parameters for pg_isready in file $(greadlink -f $0)."
      exit 3;
      ;;
  esac;
done;
