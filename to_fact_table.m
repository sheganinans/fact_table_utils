:- module to_fact_table.

:- interface.

:- import_module io.
:- import_module list.

:- func to_fact_table_str(list(T)) = string <= to_fact(T).

:- pred write_fact_table(string::in, list(T)::in, io::di, io::uo) is det <= to_fact(T).

:- typeclass to_atom(T) where [ func to_atom(T) = string ].

:- instance to_atom(string).
:- instance to_atom(int).
:- instance to_atom(float).

:- typeclass to_body(T) where [ func to_body(T) = string ].

:- instance to_body({A}) <= to_atom(A).
:- instance to_body({A,B}) <= (to_atom(A), to_atom(B)).
:- instance to_body({A,B,C}) <= (to_atom(A), to_atom(B), to_atom(C)).
:- instance to_body({A,B,C,D}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D)).
:- instance to_body({A,B,C,D,E}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E)).
:- instance to_body({A,B,C,D,E,F}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F)).
:- instance to_body({A,B,C,D,E,F,G}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G)).
:- instance to_body({A,B,C,D,E,F,G,H}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H)).
:- instance to_body({A,B,C,D,E,F,G,H,I}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I)).
:- instance to_body({A,B,C,D,E,F,G,H,I,J}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J)).
:- instance to_body({A,B,C,D,E,F,G,H,I,J,K}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K)).
:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L)).
:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L,M}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L), to_atom(M)).
:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L,M,N}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L), to_atom(M), to_atom(N)).
:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L,M,N,O}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L), to_atom(M), to_atom(N), to_atom(O)).

:- typeclass to_fact(T) where [ func to_fact(T) = string ].

:- implementation.

:- import_module string.

:- instance to_atom(string) where [ to_atom(S) = "\"" ++ S ++ "\"" ].
:- instance to_atom(int)    where [ to_atom(I) = int_to_string(I) ].
:- instance to_atom(float)  where [ to_atom(F) = float_to_string(F) ].

to_fact_table_str(L) = list.foldl(func(F,A) = A ++ to_fact(F), L, "").

write_fact_table(Name, Table, !IO) :-
  io.open_output(Name, StreamMaybe, !IO),
  (
    StreamMaybe = ok(Stream),
    io.write_strings(Stream, list.map(to_fact, Table), !IO)
  ;
    StreamMaybe = error(Error),
    io.write(Error, !IO)
  ).

:- func atom_c(T) = string <= to_atom(T).
atom_c(T) = to_atom(T) ++ ", ".

:- func body_str(string) = string.
body_str(S) = "(" ++ S ++ ").\n".

:- instance to_body({A}) <= to_atom(A) where [
to_body({A}) = body_str(to_atom(A))
].

:- instance to_body({A,B}) <= (to_atom(A), to_atom(B)) where [
to_body({A,B}) = body_str(atom_c(A) ++ to_atom(B))
].

:- instance to_body({A,B,C}) <= (to_atom(A), to_atom(B), to_atom(C)) where [
to_body({A,B,C}) = body_str(atom_c(A) ++ atom_c(B) ++ to_atom(C))
].

:- instance to_body({A,B,C,D}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D)) where [
to_body({A,B,C,D}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ to_atom(D))
].

:- instance to_body({A,B,C,D,E}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E)) where [
to_body({A,B,C,D,E}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ to_atom(E))
].

:- instance to_body({A,B,C,D,E,F}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F)) where [
to_body({A,B,C,D,E,F}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ to_atom(F))
].

:- instance to_body({A,B,C,D,E,F,G}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G)) where [
to_body({A,B,C,D,E,F,G}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ to_atom(G))
].

:- instance to_body({A,B,C,D,E,F,G,H}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H)) where [
to_body({A,B,C,D,E,F,G,H}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ to_atom(H))
].

:- instance to_body({A,B,C,D,E,F,G,H,I}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I)) where [
to_body({A,B,C,D,E,F,G,H,I}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ to_atom(I))
].

:- instance to_body({A,B,C,D,E,F,G,H,I,J}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J)) where [
to_body({A,B,C,D,E,F,G,H,I,J}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ atom_c(I) ++ to_atom(J))
].

:- instance to_body({A,B,C,D,E,F,G,H,I,J,K}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K)) where [
to_body({A,B,C,D,E,F,G,H,I,J,K}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ atom_c(I) ++ atom_c(J) ++ to_atom(K))
].

:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L)) where [
to_body({A,B,C,D,E,F,G,H,I,J,K,L}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ atom_c(I) ++ atom_c(J) ++ atom_c(K) ++ to_atom(L))
].

:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L,M}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L), to_atom(M)) where [
to_body({A,B,C,D,E,F,G,H,I,J,K,L,M}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ atom_c(I) ++ atom_c(J) ++ atom_c(K) ++ atom_c(L) ++ to_atom(M))
].

:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L,M,N}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L), to_atom(M), to_atom(N)) where [
to_body({A,B,C,D,E,F,G,H,I,J,K,L,M,N}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ atom_c(I) ++ atom_c(J) ++ atom_c(K) ++ atom_c(L) ++ atom_c(M) ++ to_atom(N))
].

:- instance to_body({A,B,C,D,E,F,G,H,I,J,K,L,M,N,O}) <= (to_atom(A), to_atom(B), to_atom(C), to_atom(D), to_atom(E), to_atom(F), to_atom(G), to_atom(H), to_atom(I), to_atom(J), to_atom(K), to_atom(L), to_atom(M), to_atom(N), to_atom(O)) where [
to_body({A,B,C,D,E,F,G,H,I,J,K,L,M,N,O}) = body_str(atom_c(A) ++ atom_c(B) ++ atom_c(C) ++ atom_c(D) ++ atom_c(E) ++ atom_c(F) ++ atom_c(G) ++ atom_c(H) ++ atom_c(I) ++ atom_c(J) ++ atom_c(K) ++ atom_c(L) ++ atom_c(M) ++ atom_c(N) ++ to_atom(O))
].
