unit FolderItem;

interface

type

	TFolderItem = class

  public

  	filePath: String;
  	constructor Create(const filePath: String);

  end;



implementation



constructor TFolderItem.Create(const filePath: String);
begin
	self.filePath := filePath;
end;


end.
