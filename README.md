# Mercury Fact Table Utils

A set of typeclasses to turn your internal Mercury types into fact tables.

## Example

```mercury
:- import_module to_fact_table.

% Some internal data we want to use as a fact in a fact table
:- type test_data --->
  test_data(
    one :: string,
    two :: int,
    three :: float).

% Just implement to_fact on your data and everything else is free.
% `to_body` takes tuples with strings, ints, and floats (currently up to 15 in length)
% Then just append the name of the fact, in this case: 'test_data' to form the head of the fact 
:- instance to_fact(test_data) where [
  to_fact(TD) = "test_data" ++ to_body({one(TD), two(TD), three(TD)})
].

% After you've implemented `to_fact` you can now use all the functions that take a list of facts.
:- pred write_example_facts(io::di, io::uo) is det.
write_example_facts(!IO) :-
  Ft = [
    test_data("1", 1, 1.0),
    test_data("2", 2, 2.0),
    test_data("3", 3, 3.0),
    test_data("4", 4, 4.0),
    test_data("5", 5, 5.0)
  ],
  io.write_string(to_fact_table_str(Ft), !IO),
  to_fact_table.write_fact_table("ft_data", Ft, !IO).

% Definitions required to read the data out of the fact table
:- pred test_data(string::out, int::out, float::out) is multi.
:- pragma fact_table(test_data/3, "ft_data").

main(!IO) :-
  write_example_facts(!IO),

  % Query the fact table returning the first field along with the third field multiplied by 4.0
  % for rows where the two field is greater than 3
  solutions(
    (pred({One, Three*4.0}::out) is nondet :-
      test_data(One,Two,Three), Two > 3),
    RowsGt3),

  io.write(RowsGt3, !IO),
io.nl(!IO).
```

## TODO

Use string.builder instead of string concatenation.
