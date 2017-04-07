
-author("silviu.caragea").

-type xmlattr() :: {binary(), binary()}.

-record(xmlcdata, {content = [] :: iodata()}).
-record(xmlel, {name :: binary(), attrs = [] :: [xmlattr()], children =  [] :: [#xmlel{} | #xmlcdata{}]}).

-type xmlterm() :: #xmlel{} | xmlattr() | #xmlcdata{}.
-type erlxml_option():: {skip_root, boolean()} | {stanza_limit, non_neg_integer()}.
-type reason() :: invalid_stanza | max_stanza_limit_hit | badarg | binary().
