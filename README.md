# Car workshop REST API

## Running the app
```bash
$ make run
```

## Notes
If you would like to create new tables.

Go into _bin_ folder
```bash
$ cd bin
```

Make _create_table_ script executable
```bash
$ chmod u+x ./bin/create_table.escript
```

Run _create_table_ script to create tables
```bash
$ ./create_table.escript ../data/inspections
$ ./create_table.escript ../data/reservations
```
