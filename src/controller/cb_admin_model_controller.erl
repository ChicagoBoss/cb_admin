-module(cb_admin_model_controller, [Req, SessionID]).
-compile(export_all).
-default_action(model).

-define(RECORDS_PER_PAGE, 100).

before_(_) ->
    cb_admin_lib:require_ip_address(Req).

heartbeat('POST', [WatchName], Authorization) ->
    boss_news:extend_watch(list_to_integer(WatchName)),
    {output, ""}.

watch('POST', [], Authorization) ->
    TopicString = Req:post_param("topic_string"),
    {ok, WatchId} = boss_news:watch(TopicString, fun cb_admin_lib:push_update/3, "admin"++SessionID, 60), 
    {json, [{watch_id, WatchId}]}.

events('GET', [Since], Authorization) ->
    io:format("Pulling events...~n", []),
    {ok, Timestamp, Messages} = boss_mq:pull("admin" ++ SessionID, list_to_integer(Since), 30),
    {json, [{messages, Messages}, {timestamp, Timestamp}]}.

model('GET', [], Authorization) ->
    {ok, [{model_section, true}, {records, []}, 
            {models, boss_web:get_all_models()}, 
            {this_model, ""}, {topic_string, ""},
            {timestamp, now()}]};
model('GET', [ModelName], Authorization) ->
    model('GET', [ModelName, "1"], Authorization);
model('GET', [ModelName, PageName], Authorization) ->
    Page = list_to_integer(PageName),
    Model = list_to_atom(ModelName),
    RecordCount = boss_db:count(Model),
    Records = boss_db:find(Model, [], [{limit, ?RECORDS_PER_PAGE}, 
            {offset, (Page - 1) * ?RECORDS_PER_PAGE}, descending]),
    TopicString = string:join(lists:map(fun(Record) -> Record:id() ++ ".*" end, Records), ", "),
    AttributesWithDataTypes = lists:map(fun(Record) ->
                {Record:id(), lists:map(fun({Key, Val}) ->
                            {Key, Val, boss_db:data_type(Key, Val)}
                    end, Record:attributes())}
        end, Records),
    AttributeNames = case length(Records) of
        0 -> [];
        _ -> (lists:nth(1, Records)):attribute_names()
    end,
    Pages = lists:seq(1, ((RecordCount-1) div ?RECORDS_PER_PAGE)+1),
    {ok, 
        [{records, AttributesWithDataTypes}, {attribute_names, AttributeNames}, 
            {models, boss_web:get_all_models()}, {this_model, ModelName}, 
            {pages, Pages}, {this_page, Page}, {model_section, true},
            {topic_string, TopicString}, {timestamp, boss_mq:now("admin"++SessionID)}], 
        [{"Cache-Control", "no-cache"}]}.

csv('GET', [ModelName], Authorization) ->
    Model = list_to_atom(ModelName),
    [First|_] = Records = boss_db:find(Model, [], [descending]),
    FirstLine = [lists:foldr(fun
                (Attr, []) ->
                    [atom_to_list(Attr)];
                (Attr, Acc) ->
                    [atom_to_list(Attr), ","|Acc]
            end, [], First:attribute_names()), "\n"],
    RecordLines = lists:map(fun(Record) ->
                [lists:foldr(fun
                            ({_Key, Val}, []) ->
                                [cb_admin_model_lib:encode_csv_value(Val)];
                            ({_Key, Val}, Acc) ->
                                [cb_admin_model_lib:encode_csv_value(Val), ","|Acc]
                        end, [], Record:attributes()), "\n"]
        end, Records),
    {output, [FirstLine, RecordLines], [{"Content-Type", "text/csv"}, 
            {"Content-Disposition", "attachment;filename="++ModelName++".csv"}]}.

upload('GET', [ModelName], Authorization) ->
    Module = list_to_atom(ModelName),
    DummyRecord = boss_record_lib:dummy_record(Module),
    {ok, [{type, ModelName}, {attributes, DummyRecord:attribute_names()}]};
upload('POST', [ModelName], Authorization) ->
    Module = list_to_atom(ModelName),
    DummyRecord = boss_record_lib:dummy_record(Module),
    [{uploaded_file, FileName, Location, Length}] = Req:post_files(),
    {ok, FileBytes} = file:read_file(Location),
    [Head|Rest] = cb_admin_model_lib:parse_csv(FileBytes),
    RecordsToSave = lists:map(fun(Line) ->
                {_, Record} = lists:foldl(fun(Val, {Counter, Acc}) ->
                            AttrName = lists:nth(Counter, Head),
                            Attr = list_to_atom(AttrName),
                            {Counter + 1, Acc:set(Attr, Val)}
                    end, {1, DummyRecord}, Line),
                Record
        end, Rest),
    % TODO put these in a transaction
    lists:foldl(fun(Record, ok) ->
                {ok, _} = Record:save(),
                ok
        end, ok, RecordsToSave),
    {redirect, [{action, "model"}, {model_name, ModelName}]}.

show('GET', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    AttributesWithDataTypes = lists:map(fun({Key, Val}) ->
                {Key, Val, boss_db:data_type(Key, Val)}
        end, Record:attributes()),
    {ok, [{'record', Record}, {'attributes', AttributesWithDataTypes}, 
            {'type', boss_db:type(RecordId)}, {timestamp, boss_mq:now("admin"++SessionID)}]}.

edit('GET', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    {ok, [{'record', Record}]};
edit('POST', [RecordId], Authorization) ->
    Record = boss_db:find(RecordId),
    NewRecord = lists:foldr(fun
            ('id', Acc) ->
                Acc;
            (Attr, Acc) ->
                AttrName = atom_to_list(Attr),
                Val = Req:post_param(AttrName),
                case lists:suffix("_time", AttrName) of
                    true ->
                        case Val of "now" -> Acc:set(Attr, erlang:now());
                            _ -> Acc
                        end;
                    false -> Acc:set(Attr, Val)
                end
        end, Record, Record:attribute_names()),
    case NewRecord:save() of
        {ok, SavedRecord} ->
            {redirect, [{action, "show"}, {record_id, RecordId}]};
        {error, Errors} ->
            {ok, [{errors, Errors}, {record, NewRecord}]}
    end.

delete('GET', [RecordId], Authorization) ->
    {ok, [{'record', boss_db:find(RecordId)}]};
delete('POST', [RecordId], Authorization) ->
    Type = boss_db:type(RecordId),
    boss_db:delete(RecordId),
    {redirect, [{action, "model"}, {model_name, atom_to_list(Type)}]}.

create(Method, [RecordType], Authorization) ->
    case lists:member(RecordType, boss_web:get_all_models()) of
        true ->
            Module = list_to_atom(RecordType),
            DummyRecord = boss_record_lib:dummy_record(Module),
            case Method of
                'GET' ->
                    {ok, [{type, RecordType}, {'record', DummyRecord}]};
                'POST' ->
                    Record = lists:foldr(fun
                                ('id', Acc) -> Acc:set(id, 'id');
                                (Attr, Acc) ->
                                    AttrName = atom_to_list(Attr),
                                    Val = Req:post_param(AttrName),
                                    Val1 = case lists:suffix("_time", AttrName) of
                                        true ->
                                            case Val of
                                                "now" -> erlang:now();
                                                _ -> ""
                                            end;
                                        _ -> Val
                                    end,
                                    Acc:set(Attr, Val1)
                            end, DummyRecord, DummyRecord:attribute_names()),
                    case Record:save() of
                        {ok, SavedRecord} ->
                            {redirect, [{action, "show"}, {record_id, SavedRecord:id()}]};
                        {error, Errors} ->
                            {ok, [{errors, Errors}, {type, RecordType}, {'record', Record}]}
                    end
            end;
        _ ->
            {error, "Nonesuch model."}
    end.

