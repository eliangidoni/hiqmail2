-module(makedoc).
-export([start/1]).

-define(LINK_DOCUMENT_TYPES, [".txt", ".pdf", ".html"]).
-define(LINK_IMAGE_TYPES, [".png", ".jpg"]).
-define(EDOC_FILE, "edoc-info").
-define(HTML_HEADER, "<head>
<title>Project Documentation</title>
<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  .title  { text-align: center; }
   body{text-align:center;}
   div#content{margin:0 auto; text-align:left; width:800px;}
   html{font-family: Georgia, times; font-size:10pt;}
   ul {list-style-type: none;}
   ul li {padding-bottom: 1%;}
   a {color:blue;}
  /*]]>*/-->
</style>
</head>").

start([AppsDir]) ->
    RootPath = filename:dirname(filename:absname("")),
    AppPath = filename:absname(AppsDir),
    Dirs = lists:filter(fun(E) -> filelib:is_dir(E) end, filelib:wildcard(AppPath++"/*")),
    gen_docs(Dirs, RootPath),
    gen_index(".").

gen_docs(Dirs, RootPath) ->
    EdocPaths = lists:filter(fun(E) -> filelib:is_dir(E) end,
                             lists:map(fun(E) -> RootPath ++ "/docs/" ++ filename:basename(E)
                                       end, Dirs)),
    lists:foreach(fun(AppDir) -> App = filename:basename(AppDir),
                                 edoc:application(list_to_atom(App),
                                                  AppDir,
                                                  [{doc_path, EdocPaths},
                                                   {dir, RootPath ++ "/docs/" ++ App},
                                                   {def, {root,RootPath}}]) end,
                  lists:filter(fun(E) -> string:substr(filename:basename(E),1,3) == "hiq" end,
                               Dirs)).

gen_index(Dir) ->
    Dirs = lists:filter(fun(E) -> filelib:is_dir(E) end, filelib:wildcard(Dir++"/*")),
    Files = lists:filter(fun(E) -> filelib:is_regular(E) end, filelib:wildcard(Dir++"/*")),
    {Docs, Imgs} = links_for_files(Files),
    {Docs2, Imgs2, Edocs} = links_for_dirs(Dirs),
    Formatted = format_links(Docs++Docs2, Imgs++Imgs2, Edocs),
    Data = ("<html>" ++ ?HTML_HEADER ++ "<body><div id=\"content\">" ++
                "<h1 class=\"title\">Project Documentation</h1>" ++ Formatted ++
                "</div></body></html>"),
    ok = file:write_file("index.html", Data).

format_links(Documents, Images, Edocs) ->
    DocsHTML = lists:foldl(fun(L, HTML) ->
                                   L2 = beautify(filename:basename(L)),
                                   "<li><a href=\"" ++ L ++ "\"/>" ++ L2 ++ "</a></li>" ++ HTML
                           end, "", lists:reverse(Documents)),
    ImgsHTML = lists:foldl(fun(L, HTML) ->
                                   L2 = beautify(filename:basename(L)),
                                   "<li><a href=\"" ++ L ++ "\"/>" ++ L2 ++ "</a></li>" ++ HTML
                           end, "", lists:reverse(Images)),
    EdocsHTML = lists:foldl(fun(L, HTML) ->
                                    L2 = beautify(filename:basename(filename:dirname(L))),
                                    "<li><a href=\"" ++ L ++ "\"/>" ++ L2 ++ "</a></li>" ++ HTML
                            end, "", Edocs),
    HTML1 = "<h2>Documents</h2>" ++ "<ul>" ++ DocsHTML ++ "</ul>",
    HTML3 = "<h2>Images</h2>" ++ "<ul>" ++ ImgsHTML ++ "</ul>",
    HTML2 = "<h2>Applications</h2>" ++ "<ul>" ++ EdocsHTML ++ "</ul>",
    HTML1 ++ HTML2 ++ HTML3.

beautify(FileName) ->
    lists:map(fun([C|Word]) -> [string:to_upper(C)] ++ Word ++ " " end,
              string:tokens(string:to_lower(filename:rootname(FileName)), " -_")).

links_for_dirs(Dirs) ->
    {Edocs, Rest} = lists:foldl(fun(Dir, {Edocs, Rest}) ->
                                        case parse_dir(Dir) of
                                            edoc -> {[Dir] ++ Edocs, Rest};
                                            Files -> {Edocs, Files ++ Rest}
                                        end end, {[],[]}, Dirs),
    {DocLinks, ImgLinks} = links_for_files(Rest),
    {DocLinks, ImgLinks, lists:map(fun(E) -> E ++ "/index.html" end, Edocs)}.

parse_dir(Dir) ->
    Files = lists:filter(fun(E) -> filelib:is_regular(E) end, filelib:wildcard(Dir++"/*")),
    case (string:substr(filename:basename(Dir),1,3) == "hiq" andalso
          lists:any(fun(E) -> filename:basename(E) == ?EDOC_FILE end, Files)) of
        true -> edoc;
        false -> Files
    end.

links_for_files(Files) ->
    {DocLinks, Rest} = lists:partition(fun(File) -> filter_type(File, ?LINK_DOCUMENT_TYPES) end,
                                       Files),
    {ImgLinks, _} = lists:partition(fun(File) -> filter_type(File, ?LINK_IMAGE_TYPES) end,
                                        Rest),
    {lists:filter(fun(E) -> not lists:suffix("index.html",E) end, DocLinks), ImgLinks}.

filter_type(File, Extensions) ->
    lists:any(fun(S) -> filename:extension(File) == S end, Extensions).
