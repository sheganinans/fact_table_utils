:- module example.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

:- import_module to_fact_table.

:- type test_data --->
  test_data(
    one :: string,
    two :: int,
    three :: float).

:- instance to_fact(test_data) where [
  to_fact(TD) = "test_data" ++ to_body({one(TD), two(TD), three(TD)})
].

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

:- pred test_data(string::out, int::out, float::out) is multi.
:- pragma fact_table(test_data/3, "ft_data").

main(!IO) :-
  write_example_facts(!IO),

  solutions(
    (pred({One, Three*4.0}::out) is nondet :-
      test_data(One,Two,Three), Two > 3),
    RowsGt3),

  io.write(RowsGt3, !IO),
  io.nl(!IO).
