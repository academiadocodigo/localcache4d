unit LocalCache4D.Interfaces;

interface

uses
  System.Generics.Collections;

type
  iLocalCache4D = interface
    ['{1E1C947B-3DD3-4693-9D20-7C06D2AA0DCF}']
    function LoadDatabase ( aDabaseName : String = '' ) : iLocalCache4D;
    function SaveToStorage (aDabaseName : String = '' ) : iLocalCache4D;
    function Instance ( aValue : String ) : iLocalCache4D;
    function RemoveInstance ( aValue : String ) : iLocalCache4D;
    function ListItens : TDictionary<string, string>;
    function ListInstances : TDictionary<String, TDictionary<string, string>>;
    function TryGetItem (aItem : String; out aResult : String) : Boolean;
    function GetItem ( aItem : String ) : string;
    function SetItem( aKey : String; aValue : String ) : iLocalCache4D;
    function RemoveItem (aKey : String) : iLocalCache4D;
  end;

implementation

end.
