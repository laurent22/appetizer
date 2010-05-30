/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef MainWindow_H
#define MainWindow_H

#include <MainScene.h>

namespace appetizer {

class MainWindow : public QWidget {

  Q_OBJECT

public:

  MainWindow();

public slots:

  void scene_sceneRectChanged(const QRectF& rect);

private:

  QGraphicsView* view_;
  MainScene* scene_;

protected:

  void resizeEvent(QResizeEvent* event);

};

}
#endif // MainWindow_H
