%% Copyright (c) 2011-2013, Anthony Ramine <n.oxyde@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ex_xml_c14n).
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).

%% @spec process_doc(#xmlDocument{}, [doc_entity()]) -> iolist()
process_doc(Doc = #xmlDocument{content = Content}, [Doc | NodeSet]) ->
  process_doc_content(Content, NodeSet);
process_doc(#xmlDocument{content = Content}, NodeSet) ->
  process_doc_content(Content, NodeSet).

%% @spec process_doc_content([doc_child()], [node()]) -> iolist()
%%       where child() = #xmlElement{} | #xmlComment{} | #xmlPI{}
process_doc_content(Content, NodeSet) ->
  process_doc_content1(Content, NodeSet, []).

%% @spec process_doc_content1([child()], [node()]) -> iolist()
%%       where child() = #xmlElement{} | #xmlComment{} | #xmlPI{}
process_doc_content1([E = #xmlElement{} | Content], NodeSet, Acc) ->
  {NewAcc, NewNodeSet} = process_doc_root(E, NodeSet, Acc),
  process_doc_content2(Content, NewNodeSet, NewAcc);
process_doc_content1([C = #xmlComment{} | Content], [C | NodeSet], Acc) ->
  process_doc_content1(Content, NodeSet, [$\n | process_comment(C, Acc)]);
process_doc_content1([PI = #xmlPI{} | Content], [PI | NodeSet], Acc) ->
  process_doc_content1(Content, NodeSet, [$\n | process_pi(PI, Acc)]).

%% @spec process_doc_content2([misc()], [misc()]) -> iolist()
%%       where misc() = #xmlComment{} | #xmlPI{}
process_doc_content2([C = #xmlComment{} | Content], [C | NodeSet], Acc) ->
  process_doc_content2(Content, NodeSet, process_comment(C, [$\n | Acc]));
process_doc_content2([PI = #xmlPI{} | Content], [PI | NodeSet], Acc) ->
  process_doc_content2(Content, NodeSet, process_pi(PI, [$\n | Acc]));
process_doc_content2(_Content, [], Acc) ->
  lists:reverse(Acc).

%% @spec process_doc_root(#xmlElement{}, [node()],
%%                        iolist()) -> {iolist(), [node()]}
process_doc_root(Root, NodeSet, Acc) ->
  process_element(Root, NodeSet, Acc, [], []).

%% @spec process_element(#xmlElement{}, [node()],
%%                       [namespace()], iolist()) -> {iolist(), [node()]}
process_element(E = #xmlElement{name = Name,
                                parents = Parents,
                                pos = Pos,
                                content = Content},
                [E | NodeSet], Acc, PNs) ->
  NameStr = atom_to_list(Name),
  Path = [{Name, Pos} | Parents],
  Acc1 = [NameStr, $< | Acc],
  {Acc2, NodeSet1, PNs1} = process_ns_axis(Path, PNs, NodeSet, Acc1),
  {Acc3, NodeSet2} = process_attr_axis(Path, NodeSet1, Acc2),
  Acc4 = [$> | Acc3],
  {Acc5, NodeSet3} = process_content(Content, NodeSet2, Acc4, PNs1, Path),
  {[$>, NameStr, $/, $< | Acc5], NodeSet3};
process_element(#xmlElement{name = Name,
                            parents = Parents,
                            pos = Pos,
                            content = Content}, NodeSet, Acc, PNs) ->
  Path = [{Name, Pos} | Parents],
  {_Acc1, NodeSet1, PNs1} = process_ns_axis(Path, PNs, NodeSet, []),
  {_Acc2, NodeSet2} = process_attr_axis(Path, NodeSet1, []),
  process_content(Content, NodeSet2, Acc, PNs1, Parents).

%% @spec process_ns_axis([{atom(), integer()}], [namespace()]), [node()],
%%                       iolist()) -> {iolist(), [node()], [namespace()]}
process_ns_axis(Parents, PNs, NodeSet, Acc) ->
  {PDefaultURI, NewPNs} = case PNs of
                            [{"", URI} | Tail] -> {URI, Tail};
                            _ -> {'', PNs} end,
  {DefaultURI, NewNSet, Ns} = case NodeSet of
                                [#xmlNsNode{parents = Parents, prefix = "",
                                            uri = URI1} | Tail1] ->
                                  {URI1, Tail1, [{"", URI1}]};
                                _ -> {'', NodeSet, []} end,
  Acc2 = case DefaultURI of
           PDefaultURI -> Acc;
           '' -> "\"\"=snlmx " ++ Acc;
           _ ->
             Acc1 = "\"=snlmx " ++ Acc,
             [$", process_attr_value(atom_to_list(PDefaultURI), Acc1)] end,
  process_ns_axis1(Parents, NewPNs, NewNSet, Acc2, Ns).

%% @hidden
process_ns_axis1(Parents, PNs = [N = {Prefix, URI} | NewPNs],
                 [#xmlNsNode{parents = Parents,
                             prefix = NodePrefix,
                             uri = NodeURI} | NewNSet], Acc, Ns) ->
  case NodePrefix of
    Prefix ->
      case NodeURI of
        URI -> process_ns_axis1(Parents, NewPNs, NewNSet, Acc, [N | Ns]);
        _ ->
          NewAcc = process_ns(Prefix, NodeURI, Acc),
          process_ns_axis1(Parents, NewPNs, NewNSet, NewAcc, [N | Ns]) end;
    _ when NodePrefix < Prefix ->
      NewAcc = process_ns(Prefix, URI, Acc),
      NewNs = [{NodePrefix, NodeURI} | Ns],
      process_ns_axis1(Parents, PNs, NewNSet, NewAcc, NewNs);
    _ -> process_ns_axis1(Parents, NewPNs, NewNSet, Acc, [N | Ns]) end;
process_ns_axis1(Parents, [], NSet, Acc, Ns) ->
  process_ns_axis2(Parents, NSet, Acc, Ns);
process_ns_axis1(_Parents, PNs, NSet, Acc, Ns) ->
  {Acc, NSet, lists:reverse(Ns, PNs)}.

%% @hidden
process_ns_axis2(Parents,
                 [#xmlNsNode{parents = Parents,
                             prefix = Prefix,
                             uri = URI} | NSet], Acc, Ns) ->
  NewNs = [{Prefix, URI} | Ns],
  process_ns_axis2(Parents, NSet, process_ns(Prefix, URI, Acc), NewNs);
process_ns_axis2(_Parents, NSet, Acc, Ns) ->
  {Acc, NSet, lists:reverse(Ns)}.

%% @spec process_ns(string(), atom(), iolist()) -> iolist()
process_ns("xml", 'http://www.w3.org/XML/1998/namespace', Acc) ->
  Acc;
process_ns(Prefix, URI, Acc) ->
  Acc1 = [$", $=, Prefix, $:, $s, $n, $l, $m, $x, $\s | Acc],
  Acc2 = process_attr_value(atom_to_list(URI), Acc1),
  [$" | Acc2].

%% @spec process_attr_axis([{atom(), integer()}], [node()],
%%                         iolist()) -> {iolist(), [node()]}
process_attr_axis(Parents,
                  [#xmlAttribute{name = Name,
                                 parents = Parents,
                                 value = Value} | NSet], Acc) ->
  Acc1 = [$", $=, atom_to_list(Name), $\s | Acc],
  Acc2 = process_attr_value(Value, Acc1),
  Acc3 = [$" | Acc2],
  process_attr_axis(Parents, NSet, Acc3);
process_attr_axis(_Parents, NSet, Acc) ->
  {Acc, NSet}.

%% @spec process_attr_value(string(), iolist()) -> iolist()
process_attr_value([$& | Value], Acc) ->
  process_attr_value(Value, ";pma&" ++ Acc);
process_attr_value([$< | Value], Acc) ->
  process_attr_value(Value, ";tl&" ++ Acc);
process_attr_value([$" | Value], Acc) ->
  process_attr_value(Value, ";tuoq&" ++ Acc);
process_attr_value([16#9 | Value], Acc) ->
  process_attr_value(Value, ";9x#&" ++ Acc);
process_attr_value([16#A | Value], Acc) ->
  process_attr_value(Value, ";Ax#&" ++ Acc);
process_attr_value([16#D | Value], Acc) ->
  process_attr_value(Value, ";Dx#&" ++ Acc);
process_attr_value([C | Value], Acc) ->
  process_attr_value(Value, [C | Acc]);
process_attr_value([], Acc) ->
  Acc.

%% @spec process_content([node()], [node()], iolist(), [namespace()]
%%                       [{atom(), integer()}]) -> {iolist(), [node()]}
process_content([E = #xmlElement{parents = Parents} | Content],
                NodeSet, Acc, PNs, Parents) ->
  {NewAcc, NewNodeSet} = process_element(E, NodeSet, Acc, PNs),
  process_content(Content, NewNodeSet, NewAcc, PNs, Parents);
process_content([C = #xmlComment{parents = Parents} | Content],
                [C | NodeSet], Acc, PNs, Parents) ->
  process_content(Content, NodeSet, process_comment(C, Acc), PNs, Parents);
process_content([PI = #xmlPI{parents = Parents} | Content], [PI | NodeSet],
                Acc, PNs, Parents) ->
  process_content(Content, NodeSet, process_pi(PI, Acc), PNs, Parents);
process_content([T = #xmlText{parents = Parents} | Content], [T | NodeSet],
                Acc, PNs, Parents) ->
  process_content(Content, NodeSet, process_text(T, Acc), PNs, Parents);
process_content([_Node | Content], NodeSet, Acc, PNs, Parents) ->
  process_content(Content, NodeSet, Acc, PNs, Parents);
process_content([], NodeSet, Acc, _PNs, _Parents) ->
  {Acc, NodeSet}.

%% @spec process_comment(#xmlComment{}, iolist()) -> iolist()
process_comment(#xmlComment{value = Value}, Acc) ->
  [$>, $-, $-, Value, $-, $-, $!, $< | Acc].

%% @spec process_pi(#xmlPI{}, iolist()) -> iolist()
process_pi(#xmlPI{name = Name, value = Value}, Acc) ->
  [$>, $? | process_pi_value(Value, [atom_to_list(Name), $?, $< | Acc])].

%% @spec process_pi_value(string(), iolist()) -> string()
process_pi_value("", Acc) ->
  Acc;
process_pi_value(Value, Acc) ->
  [Value, $\s | Acc].

%% @spec process_text(#xmlText{}, iolist()) -> iolist()
process_text(#xmlText{value = Value}, Acc) ->
  process_text1(Value, Acc).

%% @hidden
process_text1([$& | Value], Acc) ->
  process_text1(Value, ";pma&" ++ Acc);
process_text1([$< | Value], Acc) ->
  process_text1(Value, ";tl&" ++ Acc);
process_text1([$> | Value], Acc) ->
  process_text1(Value, ";tg&" ++ Acc);
process_text1([16#D | Value], Acc) ->
  process_text1(Value, ";Dx#&" ++ Acc);
process_text1([C | Value], Acc) ->
  process_text1(Value, [C | Acc]);
process_text1([], Acc) ->
  Acc.

%% @spec encode_char(char()) -> binary()
encode_char($&) ->
  <<"&amp;">>;
encode_char($<) ->
  <<"&lt;">>;
encode_char($>) ->
  <<"&gt;">>;
encode_char($") ->
  <<"&quot;">>;
encode_char(16#D) ->
  <<"&#xD;">>;
encode_char(16#A) ->
  <<"&#xA;">>;
encode_char(16#9) ->
  <<"&#x9;">>.

%% @spec default_xpath(mode()) -> list()
default_xpath(uncommented) ->
  "(//. | //@* | //namespace::*)[not(self::comment())]";
default_xpath(commented) ->
  "(//. | //@* | //namespace::*)".

%% @spec sort_nodes([node()]) -> [node()]
sort_nodes(NodeSet) ->
  lists:sort(fun is_ordered/2, NodeSet).

%% @spec is_ordered(node(), node()) -> boolean()
is_ordered(A, B) ->
  node_key(A) < node_key(B).

%% node_key(node()) -> tuple()
node_key(#xmlDocument{}) ->
  [];
node_key(#xmlElement{parents = Parents, pos = Pos}) ->
  [positions(Parents, {3, Pos})];
node_key(#xmlAttribute{name = Name, nsinfo = NSInfo, namespace = Namespace,
                       parents = Parents, pos = Pos}) ->
  AttrKey = attr_key(Name, NSInfo, Namespace),
  [positions(Parents, {2, Pos}), AttrKey];
node_key(#xmlNsNode{parents = Parents, pos = Pos, prefix = Prefix}) ->
  [positions(Parents, {1, Pos}), Prefix];
node_key(#xmlPI{parents = Parents, pos = Pos}) ->
  [positions(Parents, {3, Pos})];
node_key(#xmlComment{parents = Parents, pos = Pos}) ->
  [positions(Parents, {3, Pos})];
node_key(#xmlText{parents = Parents, pos = Pos}) ->
  [positions(Parents, {3, Pos})].

%% positions([{name(), integer()}],
%%           {integer(), integer()}) -> [{integer(), integer()}]
positions(Parents, Pos) ->
  positions1(Parents, [Pos]).

%% @hidden
positions1([{_Name, Pos} | Parents], Acc) ->
  positions1(Parents, [{3, Pos} | Acc]);
positions1([], Acc) ->
  Acc.

%% @spec attr_key(atom(), nsinfo(), #xmlNamespace{}) -> {atom(), atom()}
%%       where nsinfo() = [] | {string(), string()}
attr_key(Name, "", #xmlNamespace{default = []}) ->
  {'', Name};
attr_key(Name, "", #xmlNamespace{default = Default}) ->
  {Default, Name};
attr_key(_Name, {Prefix, Local}, #xmlNamespace{nodes = Nodes}) ->
  {_Prefix, URI} = lists:keyfind(Prefix, 1, Nodes),
  {URI, list_to_atom(Local)}.
