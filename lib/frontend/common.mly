%%

%public ident:
	| w = WORD { Identifier w : identifier }
