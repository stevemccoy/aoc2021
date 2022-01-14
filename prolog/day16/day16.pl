%
% Advent of Code 2021. Day 16, Part 1.
%

:- dynamic lit_packet/1.
:- dynamic op_packet/1.

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% DCG Grammar for bit string encodings.

packets --> packet(_, _, _, _, _).
packets --> packet(_, _, _, _, _), packets.

% Decode packets into the following:
% Version, Type, Value, SubPackets, BitsConsumed

packet(Ver, 4, Value, [], BitsConsumed) -->
	literal_packet(Ver, Value, BitsConsumed),
	{	make_packet(Ver, 4, Value, [], P),
		assertz(lit_packet(P))
	}.

packet(Ver, Type, Value, SubPackets, BitsConsumed) -->
	operator_packet(Ver, Type, SubPackets, BitsConsumed),
	{	Type \= 4,
		make_packet(Ver, Type, Value, SubPackets, P),
		assertz(op_packet(P))
	}.

packet_header(Version, Type) --> packet_version(Version), packet_type(Type).

packet_version(Ver) --> triple(Ver).

packet_type(Ver) --> triple(Ver).

literal_packet(Version, Value, BitsConsumed) -->
	packet_header(Version, 4),
	value_groups(ValueList, BC1),
	{	evaluate_nibble_sequence(ValueList, Value),
		BitsConsumed is BC1 + 6
	}.

operator_packet(Version, Type, SubPackets, BitsConsumed) -->
	packet_header(Version, Type),
	sub_packet_header(BitLength, PacketCount, BC2),
	sub_packets(BitLength, PacketCount, SubPackets, BC1),
	{
		Type \= 4,
		BitsConsumed is BC1 + BC2 + 6
	}.

sub_packet_header(BitLength, _, 16) -->
	['0'], bit_string(15, BitString),
	{
		bit_string_value(BitString, BitLength)
	}.
sub_packet_header(_, PacketCount, 12) -->
	['1'], bit_string(11, BitString),
	{
		bit_string_value(BitString, PacketCount)
	}.

% Sub-packets by count.
% sub_packets(BitLength, PacketCount, SubPackets, BitsConsumed)
sub_packets(BitLength, PacketCount, [], 0) --> [],
	{	var(BitLength),
		integer(PacketCount),
		PacketCount == 0
	}.
sub_packets(BitLength, PacketCount, [P | Tail], BitsConsumed) -->
	packet(Ver, Type, Value, SPL1, BC1), 
	sub_packets(BitLength, PC2, Tail, BC2),
	{	var(BitLength), 
		integer(PacketCount),
		PacketCount > 0,
		make_packet(Ver, Type, Value, SPL1, P),
		PC2 is PacketCount - 1,
		BitsConsumed is BC1 + BC2
	}.

% Sub-packets by bit length.
sub_packets(BitLength, PacketCount, [], 0) --> [],
	{	var(PacketCount),
		integer(BitLength),
		BitLength =< 0
	}.
sub_packets(BitLength, PacketCount, [P | Tail], BitsConsumed) -->
	packet(Ver, Type, Value, SPL1, BC1),
	sub_packets(BL2, PacketCount, Tail, BC2),
	{	var(PacketCount), 
		integer(BitLength),
		BitLength > 0,
		make_packet(Ver, Type, Value, SPL1, P),
		BL2 is BitLength - BC1,
		BitsConsumed is BC1 + BC2
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
/*	{	N > 1,
		M is N - 1
	}.
*/

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

make_packet(Ver, Type, Value, SubPackets, [v(Ver), t(Type), val(Value), SubPackets]).

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
	read_input_data(FileName, BitString),
	string_chars(BitString, Chars),
	once(phrase(packets, Chars, [])).

day16_part1 :-
	writeln('Advent of Code 2021.\nDay 16, Part 1.').

