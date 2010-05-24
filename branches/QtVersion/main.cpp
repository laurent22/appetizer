#include <stable.h>
#include <Application.h>
#include <IconData.h>
#include <IconUtil.h>
#include <FilePaths.h>
#include <NineSliceItem.h>
#include <XmlUtil.h>
#include <User.h>
#include <IconPanel.h>
using namespace appetizer;


int main(int argc, char *argv[]) {
  Application app(argc, argv);
  app.setOrganizationName("Appetizer Project");
  app.setApplicationName("Appetizer");

  //QString t = FilePaths::GetUserShellDirectory("Programs");

  


//  QFileInfo f("%APPDATA%\\Microsoft\\Internet Explorer\\Quick Launch");
//  QString d = f.canonicalFilePath();
//

  //appetizer::XmlUtil::test(1);

  //QGraphicsScene scene;

  //NineSliceItem item;
  //item.loadBackgroundImage("c:\\Users\\Laurent_2\\Desktop\\200.PNG");

  //scene.addItem(&item);

  //scene.addText("Hello, world!");

  //IconData iconData = IconUtil::getFolderItemIcon("C:\\Program Files\\Adobe\\Adobe Photoshop CS4\\Photoshop.exe", 256);
  //scene.addPixmap(QPixmap::fromWinHICON(iconData.hIcon));

  //iconData = IconUtil::getFolderItemIcon("C:\\Program Files\\Adobe\\Adobe Illustrator CS4\\Support Files\\Contents\\Windows\\AI_Application_Icon.ico", 48);
  //scene.addPixmap(QPixmap::fromWinHICON(iconData.hIcon));

  //QGraphicsView view(&scene); 
  //view.show();

  return app.exec();
}