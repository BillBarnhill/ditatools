-module(mg_graphmaker).

-export([process_file/1, print/1, ditamap/2, printing_stream/0]).


-record(entry, {indent, position, title}).
-record(acc, {graph, current=none}).

process_file(SrcFile) ->
	Graph = digraph:new([acyclic]),
	Current = digraph:add_vertex(Graph), % root
	process_file(SrcFile, #acc{graph=Graph,current=Current}).

process_file(SrcFile, Acc0) ->
	{ok, Src} = file:open(SrcFile, [raw, read, read_ahead]),
	Acc = process_line(Src, file:read_line(Src), Acc0),
	file:close(Src),
	Acc.
	
process_line(_, eof, Acc) ->
	Acc;

process_line(Src, {ok, Line}, Acc0) ->
	Graph0 = Acc0#acc.graph,
	Current0 = Acc0#acc.current,
	{Graph, Current} = process_outline_line(Graph0, Current0, Line),
	AccNext = #acc{graph=Graph, current=Current},
	process_line(Src, file:read_line(Src), AccNext).

%% Start of outline specific processing
%% Returns {Graph, Current}
process_outline_line(Graph0, Current0, Line) ->
	Indent = count_indent(Line),
	Title = parse_title(Line),
	LastIndent = last_indent(Graph0,Current0),
	{Type, Steps} = if
			LastIndent == -1 -> {first, undefined};
			LastIndent > Indent -> {up, LastIndent - Indent};
			LastIndent < Indent -> {down, 1};
			LastIndent == Indent -> {sibling, 0}
		end,
	handle_type(Type, Steps, Graph0, Current0, Indent, Title). 

handle_type(first, _, Graph, Current0, Indent, Title) ->
	Parent = Current0, % Current is initially the virtual root
	Position = 1,
	Current = add_child(Graph, Parent, Indent, Position, Title),
	{Graph, Current};	

handle_type(up, Steps, Graph, Current0, Indent, Title) ->
	Parent = get_ancestor(Steps+1, Graph, Current0),
	Position = digraph:out_degree(Graph, Parent)+1,
	Current = add_child(Graph, Parent, Indent, Position, Title),
	{Graph, Current};
 	

handle_type(down, _, Graph, Current0, Indent, Title) ->
	Parent = Current0,
	Position = 1,
	Current = add_child(Graph, Parent, Indent, Position, Title),
	{Graph, Current};

handle_type(sibling, _, Graph, Current0, Indent, Title) ->
	Parent = get_parent(Graph,Current0),
	Position = digraph:out_degree(Graph, Parent)+1,
	Current = add_child(Graph, Parent, Indent, Position, Title),
	{Graph, Current}.


add_child(Graph, Parent, Indent, Position, Title ) ->
	V = digraph:add_vertex(Graph),
	PositionList = [Position | position(Graph, Parent)],
	Entry = #entry{indent=Indent, position=PositionList, title=Title},
	digraph:add_vertex(Graph, V, Entry),
	digraph:add_edge(Graph, Parent, V),
	V.

get_ancestor(0, _, Current) ->
	Current;
		
get_ancestor(N, Graph, Current) ->
	get_ancestor(N-1, Graph, get_parent(Graph,Current)).

get_parent(Graph, Current) ->
	[Parent] = digraph:in_neighbours(Graph, Current),
	Parent.

parse_title(Line) ->
	AfterIndent = lists:dropwhile(fun 
				  	($\t) -> true;
			  		(_) -> false
				      end, Line),
	Title = lists:takewhile(fun
					($\n) -> false;
					(_) -> true
				end, AfterIndent),
	Title.

count_indent(Line) ->
	count_indent(Line, 0).

count_indent([$\t | Line], Count) ->
	count_indent(Line, Count+1);

count_indent(_, Count) ->
	Count.


last_indent(Graph,Current) ->
	case digraph:vertex(Graph, Current) of
		{Current, #entry{indent=Indent}} -> Indent;
		{Current, []} -> -1
	end.

position(Graph, Vertex) ->
	case digraph:vertex(Graph, Vertex) of
		{Vertex, #entry{position=Position}} -> Position;
		{Vertex, []} -> []
	end.

entry_label(Position) ->
	[LastPos | RestPos] = Position,
	lists:flatmap(
		fun (P) -> integer_to_list(P)++"." end,
		lists:reverse(RestPos)) ++ integer_to_list(LastPos).

print_enter(_Graph, ['$v'|N], #entry{position=Position, title=Title}, State) ->
	io:format("Entry #~p-> ~p ~s~n", [N, entry_label(Position), Title]),
	State.

print_leave(_, _, _, State) ->
	State.

print(Graph) ->
	process_graph(Graph, fun print_enter/4, fun print_leave/4, []).
	
 

process_graph(Graph, FnIn, FnOut, AccIn) ->
	% We assume root is first vertex and is connector with no data
	Root = ['$v'|0],
	process_neighbours(Graph,Root, FnIn, FnOut, AccIn).

process_vertex(Graph, V, FnIn, FnOut, AccIn) ->
	{V, Data} = digraph:vertex(Graph, V),
	AccPost = FnIn(Graph, V, Data, AccIn),
	AccAfter = process_neighbours(Graph, V, FnIn, FnOut, AccPost),
	FnOut(Graph, V, Data, AccAfter).

process_neighbours(Graph, V, FnIn, FnOut, AccIn) ->
	ProcFn = fun (NV, Acc) -> process_vertex(Graph, NV, FnIn, FnOut, Acc) end,
	OrderedNeighbours = lists:reverse(digraph:out_neighbours(Graph, V)),
	lists:foldl(ProcFn, AccIn, OrderedNeighbours). 
  


ditamap(Graph, Base) when is_binary(Base)->
	process_graph(Graph, fun ditamap_enter/4, fun ditamap_leave/4, {Base, <<>>}).

ditamap_enter(_Graph, _V, #entry{position=Position, title=Title}, {Base, Out}) ->
	case length(Position) of
		1 -> 
			Pre = <<"<chapter navtitle=\"">>,
			Post = <<"\">\n">>,
			{Base, <<Out/binary, Pre/binary, (list_to_binary(Title))/binary, Post/binary>>};
		_ -> 
			TitleBin = list_to_binary(Title),
			TitleWithoutSpaces = lists:map(fun 
								($\ ) -> $_;
								(Ch) -> Ch
							end, 
							Title),
			TitleWithoutSpacesBin = list_to_binary(TitleWithoutSpaces),
			FileNameBase = <<
					Base/binary, 
					<<"-">>/binary,	
					TitleWithoutSpacesBin/binary
					>>,
			{Base,<<
				<<"\ttopicref href=\"priv/topics/">>/binary,
				FileNameBase/binary,
				<<".dita\" navtitle=\"">>/binary,
				TitleBin/binary,
				<<"\">\n">>/binary >>}
	end.



ditamap_leave(_Graph, _V, #entry{position=Position}, {Base, Out}) ->
	case length(Position) of
		1 -> 
			{Base, << Out/binary, <<"</chapter>\n">>/binary >>};
		_ -> 
			{Base, << Out/binary, <<"</topicref>\n">>/binary >>}
	end.


%xml_builder(Stream) ->
%	State = init_xml_builder(),
%	xml_builder_loop(State).


				

