-module(erlxml_utils).
-author("byron.wang").
-include("erlxml.hrl").

-export([
  cdata/1,
  subel/2,
  subel_cdata/2
]).

-spec cdata(#xmlel{}) ->
  binary().

cdata(#xmlel{children = Children}) ->
  case lists:keyfind(xmlcdata, 1, Children) of
    {xmlcdata, Xmlcdata} -> Xmlcdata;
    _ -> <<>>
  end.

-spec subel(#xmlel{}, binary()) ->
  #xmlel{} | undefined.

subel(#xmlel{children = Children}, Name) ->
  case lists:keyfind(Name, 2, Children) of
    #xmlel{} = X -> X;
    _ -> undefined
  end.

-spec subel_cdata(#xmlel{}, binary()) ->
  binary() | undefined.

subel_cdata(#xmlel{} = Xml, Name) ->
  case subel(Xml, Name) of
    #xmlel{} = X -> cdata(X);
    _ -> undefined
  end.
