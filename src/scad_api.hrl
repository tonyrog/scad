%% define api
-ifndef(__SCAD_API_HRL__).
-define(__SCAD_API_HRL__, true).

-import(scad_object,
	[scope/2,
	 cube/1,cube/2,cube/3,
	 sphere/1,sphere/2,sphere/3,
	 cylinder/2,cylinder/3,cylinder/4,
	 cone/2,cone/3,cone/4,
	 intersection/1,intersection/2,
	 difference/1,difference/2,
	 union/1,union/2,
	 color/2,
	 rotate/2,rotate/3,
	 translate/2,translate/3,
	 scale/2,scale/3]).

-endif.
