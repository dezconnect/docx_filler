
-module(docx_filler).

-export([template/2]).

%% Последовательность действий будет следующей:
%% + 1. Получаем на вход файл, и хешмеп заполненный нужныйми значениями
%% + 2. Выбираем все ключевые слова из файла 
%% + 3. Проверяем что для каждого из ключений есть значение
%% 4. Делаем подстановку всех значений по шаблону
%% + 5. Сохраняем файл

generate_exception(Reason) ->
  {error, Reason}.


template(File, Params) ->
  case readKeywords(File) of
    {ok, Keywords} ->  
      case verify(Keywords, Params) of
        true -> 
          replaceValues(File, Params);
        false -> 
          generate_exception(Reason)
      end;
    {error, Reason} ->
      generate_exception(Reason)    
  end.


readKeywords(File) ->
  case file:read_file(File) of
    {ok, Content} -> 
      case re:run(unicode:characters_to_list(Content), "\{\{([a-z0-9]+\}\})", [global, {capture, [1], list}]) of
        {match, Keywords} ->
          {ok, Keywords};
        {_} ->
          generate_exception("Cannot read template file.")
      end;
    {error, Reason} ->
      generate_exception(Reason)
  end. 


verify(Keywords, Params) ->
  lists:all(fun(K) -> lists:member(K, maps:keys(Params)) end, lists:map(fun([K]) -> list_to_atom(K) end, Keywords)).


replaceValues(File, Params) ->
  case file:read_file(File) of
    {ok, Content} -> 
      ReplaceFun = fun() end
      NewContent = re:replace(unicode:charachter_to_list(Content), )
      writeFile(File, NewContent);
    {error, Reason} -> 
      generate_exception(Reason)
  end.


writeFile(File, Content) ->
  case file:write_file(File, list_to_binary(Content)) of
    ok -> 
      {ok};
    {error, Reason} ->
      generate_exception(Reason)
  end.


