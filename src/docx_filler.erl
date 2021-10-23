
-module(docx_filler).
-export([template/3, replaceValues/2, verify/2]).


generate_exception(Reason) ->
  {error, Reason}.


template(TemplateFile, DestinationFile, Params) ->
  case readKeywords(TemplateFile) of
    {ok, Keywords} -> 
      case verify(Keywords, Params) of
        true -> 
          Content = readFile(TemplateFile),
          NewContent = replaceValues(Content, Params),
          writeFile(DestinationFile, NewContent);
        false -> 
          generate_exception("Not verified.")
      end;
    {error, Reason} ->
      generate_exception(Reason)    
  end.


readFile(File) ->
  case file:read_file(File) of
    {ok, Content} -> 
      unicode:characters_to_list(Content);
    {error, Reason} ->
      generate_exception(Reason)
  end.


%% 
%% Read all Keywords from template file
readKeywords(File) ->
  case file:read_file(File) of
    {ok, Content} -> 
      case re:run(unicode:characters_to_list(Content), "\{\{([a-z0-9]+)\}\}", [global, {capture, [1], list}]) of
        {match, Keywords} ->
          {ok, Keywords};
        {_} ->
          generate_exception("Cannot read keywords from template file.")
      end;
    {error, Reason} ->
      generate_exception(Reason)
  end. 


%% 
%% Match keys between Keywords from file and Params from input
%%
verify(Keywords, Params) ->
  lists:all(fun(K) -> lists:member(K, maps:keys(Params)) end, 
            lists:map(fun([K]) -> list_to_atom(K) end, Keywords)).


%% 
%% Take Params one by one 
%% and apply them for Content 
%% and return NewContent as Result
%% ATTENTION: Now works for strings only!
%%
replaceValues(Content, Params) when Params == #{} ->
  Content;
replaceValues(Content, Params) ->
  [ Key | _ ] = maps:keys(Params),
  { Value, NewParams} = maps:take(Key, Params),
  Result = replaceValue(Content, Key, Value),
  replaceValues(Result, NewParams).


%% 
%% ATTENTION: Now works for strings only!
%% TODO: Add another types of values
%%
replaceValue(Content, Key, Value) -> 
  ReTemplate = "\{\{" ++ atom_to_list(Key) ++ "\}\}",
  re:replace(Content, ReTemplate, Value, [{return, list}]).


writeFile(File, Content) ->
  case file:write_file(File, list_to_binary(Content)) of
    ok -> 
      {ok};
    {error, Reason} ->
      generate_exception(Reason)
  end.


