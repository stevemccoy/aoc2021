%
% Advent of Code 2021. Day 16.
%

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2021/prolog/day16/').

:- dynamic lit_packet/1.
:- dynamic op_packet/1.

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% DCG Grammar for bit string encodings.

packets --> top_packet(_).
packets --> top_packet(_), packets.

top_packet(P) --> packet(Ver, 4, Val, [], _),
	{	make_packet(Ver, 4, Val, [], P),
		assertz(P)
	}.
top_packet(P) --> packet(Ver, Type, Val, SubPackets, _),
	{	make_packet(Ver, Type, Val, SubPackets, P),
		assertz(P)
	}.

% Decode packets into the following:
% Version, Type, Value, SubPackets, BitsConsumed

packet(Ver, 4, Value, [], BitsConsumed) -->
	literal_packet(Ver, Value, BitsConsumed).

packet(Ver, Type, _, SubPackets, BitsConsumed) -->
	operator_packet(Ver, Type, SubPackets, BitsConsumed).

packet_header(Version, Type) --> packet_version(Version), packet_type(Type).

packet_version(Ver) --> triple(Ver).

packet_type(Ver) --> triple(Ver).

% Literal packet.
literal_packet(Version, Value, BitsConsumed) -->
	packet_header(Version, 4),
	value_groups(ValueList, BC1),
	{	evaluate_nibble_sequence(ValueList, Value),
		BitsConsumed is BC1 + 6
	}.

% Operator with subpackets by count.
operator_packet(Version, Type, SubPackets, BitsConsumed) -->
	packet_header(Version, Type),
	sub_packet_header(_, PacketCount, 12),
	sub_packets_by_count(PacketCount, SubPackets, BC1),
	{	Type \== 4,
		BitsConsumed is BC1 + 6 + 12
	}.

% Operator with subpackets by bit count.
operator_packet(Version, Type, SubPackets, BitsConsumed) -->
	packet_header(Version, Type),
	sub_packet_header(BitLength, _, 16),
	sub_packets_by_bits(BitLength, SubPackets, BC1),
	{	Type \== 4,
		BitsConsumed is BC1 + 6 + 16
	}.

sub_packets_by_count(0, [], 0) --> [], !.
sub_packets_by_count(PacketCount, [SP | Tail], BitsConsumed) -->
	packet(Ver, Type, Value, SPL1, BC1),
	reduce_count(PacketCount, 1, PC),
	sub_packets_by_count(PC, Tail, BC2),
	{	make_packet(Ver, Type, Value, SPL1, SP),
		BitsConsumed is BC1 + BC2
	}.

reduce_count(Count, Consumed, NewCount) --> [],
	{	Consumed =< Count,
		NewCount is Count - Consumed
	}.

sub_packets_by_bits(0, [], 0) --> [], !.
sub_packets_by_bits(BitLength, [SP | Tail], BitsConsumed) -->
	packet(Ver, Type, Value, SPL1, BC1),
	reduce_count(BitLength, BC1, BL1),
	sub_packets_by_bits(BL1, Tail, BC2),
	{	BL1 is BitLength - BC1,
		make_packet(Ver, Type, Value, SPL1, SP),
		BitsConsumed is BC1 + BC2
	}.

sub_packet_header(BitLength, _, 16) --> ['0'], bit_string(15, BitString),
	{	bit_string_value(BitString, BitLength)
	}.
sub_packet_header(_, PacketCount, 12) --> ['1'], bit_string(11, BitString),
	{	bit_string_value(BitString, PacketCount)
	}.

value_groups([Value], 5) -->
	end_group(Value),
	{}.
value_groups([Value | Tail], BitsConsumed) -->
	non_end_group(Value),
	value_groups(Tail, BC1),
	{	BitsConsumed is BC1 + 5
	}.

end_group(Value) --> ['0'], nibble(Value).

non_end_group(Value) --> ['1'], nibble(Value).

nibble(Value) --> bit(B3), bit(B2), bit(B1), bit(B0),
	{	Value is B0 + 2 * (B1 + 2 * (B2 + 2 * B3))
	}.

triple(Value) --> bit(B2), bit(B1), bit(B0),
	{	Value is B0 + 2 * (B1 + 2 * B2)
	}.

bit(0) --> ['0'].
bit(1) --> ['1'].

bit_string(1, [B]) --> bit(B).
bit_string(N, [B | Tail]) --> bit(B), bit_string(M, Tail),
	{	length(Tail, M),
		N is M + 1
	}.	

% Tot up the value of a sequence of nibbles.
evaluate_nibble_sequence([V], V).
evaluate_nibble_sequence(VL1, Value) :-
	conc(VL2, [V1], VL1),
	evaluate_nibble_sequence(VL2, V2),
	Value is V1 + V2 * 16.

% Same thing to evaluate an unsigned bit sequence.
bit_string_value([B], B).
bit_string_value(BS1, Value) :-
	conc(BS2, [B1], BS1),
	bit_string_value(BS2, V2),
	Value is B1 + V2 * 2.

make_packet(Ver, 4, Value, SubPackets, lit_packet([v(Ver), t(4), val(Value), SubPackets])) :- !.
make_packet(Ver, Type, Value, SubPackets, op_packet([v(Ver), t(Type), val(Value), SubPackets])).

hex_value('0', "0000").
hex_value('1', "0001").
hex_value('2', "0010").
hex_value('3', "0011").
hex_value('4', "0100").
hex_value('5', "0101").
hex_value('6', "0110").
hex_value('7', "0111").
hex_value('8', "1000").
hex_value('9', "1001").
hex_value('A', "1010").
hex_value('B', "1011").
hex_value('C', "1100").
hex_value('D', "1101").
hex_value('E', "1110").
hex_value('F', "1111").

hex_to_bin_list([], []).
hex_to_bin_list([HC | HT], BitList) :-
	hex_value(HC, BS1),
	string_chars(BS1, BL1),
	hex_to_bin_list(HT, BL2),
	conc(BL1, BL2, BitList).

decode_hex_string_to_binary(HexString, BitString) :-
	string_chars(HexString, L1),
	hex_to_bin_list(L1, L2),
	string_chars(BitString, L2).

read_input_line(Stream, BitString) :- 
	read_line_to_string(Stream, String),
	decode_hex_string_to_binary(String, BitString).

read_input_lines(Stream, BitString) :-
	read_input_line(Stream, BS1),
	!,
	read_input_lines(Stream, BS2),
	string_concat(BS1, BS2, BitString).
read_input_lines(_, "").

read_input_data(FileName, BitString) :-
	retractall(lit_packet(_)),
	retractall(op_packet(_)),
	open(FileName, read, Stream),
	read_input_lines(Stream, BitString),
	close(Stream).

solve_for(FileName) :-
	retractall(op_packet),
	retractall(lit_packet),
	read_input_data(FileName, BitString),
	string_chars(BitString, Chars),
	once(phrase(packets, Chars, _)).

version_sum_packet(Packet, Sum) :-
	Packet =.. [_, [v(Ver), _, _, SPL]],
	findall(VerSum, (member(P, SPL), version_sum_packet(P, VerSum)), SumList),
	sum_list([Ver | SumList], Sum).

version_sum_all_packets(Sum) :-
	findall(VerSum1, (lit_packet(PL1), version_sum_packet(lit_packet(PL1), VerSum1)), VSL1),
	findall(VerSum2, (op_packet(PL2), version_sum_packet(op_packet(PL2), VerSum2)), VSL2),
	conc(VSL1, VSL2, VSL3),
	sum_list(VSL3, Sum).

day16_part1 :-
	writeln('Advent of Code 2021.\nDay 16, Part 1.'),
	solve_for('day16.txt'),
	version_sum_all_packets(Sum),
	write('Version sum of all packets in input = '),
	writeln(Sum).

product_list([X], X) :- !.
product_list([X | Tail], Value) :-
	product_list(Tail, Y),
	Value is X * Y.

% evaluate_packet(Packet, Value).
% Literal packet.
evaluate_packet(lit_packet(L1), Value) :-
	!, 
	member(val(Value), L1).
% Sum packet.
evaluate_packet(op_packet([_, t(0), _, SPL]), Value) :-
	findall(Val, (member(P, SPL), evaluate_packet(P, Val)), VList),
	sum_list(VList, Value).
% Product packet.
evaluate_packet(op_packet([_, t(1), _, SPL]), Value) :-
	findall(Val, (member(P, SPL), evaluate_packet(P, Val)), VList),
	product_list(VList, Value).
% Minimum packet.
evaluate_packet(op_packet([_, t(2), _, SPL]), Value) :-
	findall(Val, (member(P, SPL), evaluate_packet(P, Val)), VList),
	min_list(VList, Value).
% Maximum packet.
evaluate_packet(op_packet([_, t(3), _, SPL]), Value) :-
	findall(Val, (member(P, SPL), evaluate_packet(P, Val)), VList),
	max_list(VList, Value).
% Greater than packet.
evaluate_packet(op_packet([_, t(5), _, [P1, P2]]), Value) :-
	evaluate_packet(P1, V1),
	evaluate_packet(P2, V2),
	V1 > V2 -> 
	Value is 1;
	Value is 0.
% Less than packet.
evaluate_packet(op_packet([_, t(6), _, [P1, P2]]), Value) :-
	evaluate_packet(P1, V1),
	evaluate_packet(P2, V2),
	V1 < V2 -> 
	Value is 1;
	Value is 0.
% Equals packet.
evaluate_packet(op_packet([_, t(7), _, [P1, P2]]), Value) :-
	evaluate_packet(P1, V1),
	evaluate_packet(P2, V2),
	V1 = V2 -> 
	Value is 1;
	Value is 0.

day16_part2 :-
	writeln('Advent of Code 2021.\nDay 16, Part 2.'),
	solve_for('day16.txt'),
	op_packet(P),
	evaluate_packet(op_packet(P), Value),
	write('Value of top level packet in input = '),
	writeln(Value).

