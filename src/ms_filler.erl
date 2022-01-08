%% -*- coding: utf-8 -*-

-module(ms_filler).
-export([template/3, docx_template/3, xlsx_template/3]).
-export([seedTempDirName/0]).

%%
% Function to handling undefined behaviour
% TODO: Add normal error handling 
generate_exception(Reason) ->
  {error, Reason}.


%%
% Exported function for external applications for Microsoft Word .docx files
%
docx_template(TemplateFile, DestinationFile, Params) ->
  case unzipFile(TemplateFile) of 
    {ok, DirName, FileList} -> 
      DocumentFile = DirName ++ "word/document.xml",
      % TODO: Add correct error handling 
      template(DocumentFile, DocumentFile, Params),
      zipDirectory(DestinationFile, FileList, DirName),
			deleteDirectory(DirName); 
    {error, Reason} ->
      generate_exception(Reason)
  end.


%%
% Exported funciton for external applications for Microsoft Excel .xlsx files
%
xlsx_template(TemplateFile, DestinationFile, Params) ->
  case unzipFile(TemplateFile) of
    {ok, DirName, FileList} ->
      SheetFile = DirName ++ "xl/sharedStrings.xml",
      template(SheetFile, SheetFile, Params),
      zipDirectory(DestinationFile, FileList, DirName),
      deleteDirectory(DirName);
    {error, Reason} ->
      generate_exception(Reason)
  end.


%% 
% Function for unzip template file to named directory
%
unzipFile(TemplateFile) ->
  TempDirName = seedTempDirName(),
  case zip:unzip(TemplateFile, [{ cwd, TempDirName }]) of
		{ok, FileList} ->
			{ok, TempDirName, FileList};
		{error, Reason} ->
			generate_exception(Reason)
	end.


%% 
% Zipping directory to file-container
%
zipDirectory(DestinationFile, FileList, DirName) ->
  ClearedFileList = clearFileList(FileList, DirName),
  case zip:create(DestinationFile, ClearedFileList, [{ cwd, DirName }]) of
		{ok, _} ->
			ok;
		{error, Reason} ->
			generate_exception(Reason)
	end.


%%
% Remove temp directory path from FileList
%
clearFileList(FileList, DirName) ->
  lists:map(fun(FileName) -> re:replace(FileName, DirName, "", [{return, list}]) end, FileList).


%%
% Check filetype 
%
isFileOrLink(File) -> filelib:is_regular(File) or (ok == element(1, file:read_link(File))).


%%
% Delete directory
%
deleteDirectory(DirectoryName) ->
  case {filelib:is_dir(DirectoryName), isFileOrLink(DirectoryName)} of
    {_, true} -> throw("The specified path is not a directory");
    {true, _} -> recursiveDeleteDirectory([DirectoryName]);
    {false, _} -> ok
  end.


ensureDirSlash(Dir, Child) ->
  case lists:suffix("/", Dir) of
    true -> Dir ++ Child;
    false -> Dir ++ "/" ++ Child
  end.


recursiveDeleteDirectory([]) -> ok;
recursiveDeleteDirectory([Current | Rest]) ->
	{ok, ChildrenNames} = file:list_dir(Current),
  Children = lists:map(fun(File) -> ensureDirSlash(Current, File) end, ChildrenNames),
  {FilesOnly, DirsOnly} = lists:partition(fun isFileOrLink/1, Children),
  lists:foreach(fun(File) -> ok = file:delete(File) end, FilesOnly),

  if
    length(DirsOnly) == 0 -> ok = file:del_dir(Current), recursiveDeleteDirectory(Rest);
    true -> recursiveDeleteDirectory(DirsOnly ++ [Current] ++ Rest)
  end.


%%
% Exported function for external applications for plain-text files 
%
template(TemplateFile, DestinationFile, Params) ->
  case readKeywords(TemplateFile) of
    {ok, Keywords} -> 
      case verify(Keywords, Params) of
        true -> 
          Content = unicode:characters_to_binary(readFile(TemplateFile)),
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
      unicode:characters_to_list(Content, utf8);
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


%% 
% Read all Keywords from template file
%
readKeywords(File) ->
  Content = unicode:characters_to_binary(readFile(File)), 
  case re:run(Content, "\{\{([a-zA-Z0-9\s\_]+)\}\}", [global, {capture, [1], list}]) of
    {match, Keywords} ->
      {ok, Keywords};
    {_} ->
      generate_exception("Cannot read keywords from template file.")
  end.


%% 
% Match keys between Keywords from file and Params from input
%
verify(Keywords, Params) ->
  lists:all(fun(K) -> lists:member(K, maps:keys(Params)) end, 
            lists:map(fun([K]) -> list_to_atom(K) end, Keywords)).


%% 
% Take Params one by one 
% and apply them for Content 
% and return NewContent as Result
% ATTENTION: Now works for strings only!
%
replaceValues(Content, Params) when Params == #{} ->
  Content;
replaceValues(Content, Params) ->
  [ Key | _ ] = maps:keys(Params),
  { Value, NewParams} = maps:take(Key, Params),
  Result = replaceValue(Content, Key, Value),
  replaceValues(Result, NewParams).


%% 
% ATTENTION: Now works for strings only!
% TODO: Add another types of values
%
replaceValue(Content, Key, Value) -> 
  ReTemplate = "\{\{" ++ atom_to_list(Key) ++ "\}\}",
  re:replace(Content, ReTemplate, unicode:characters_to_binary(Value), [{return, list}]).


%%
% Generate random seed for temp dir name
%
seedTempDirName() ->
  Bits = entropy_string:bits(5.0e6, 1.0e12),
  "./" ++ erlang:binary_to_list(entropy_string:random_string(Bits)) ++ "/".



