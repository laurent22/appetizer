/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef AZMAINWINDOW_H
#define AZMAINWINDOW_H

#include <azMainScene.h>

class azMainWindow : public QGraphicsView {

public:

  azMainWindow();

private:

  azMainScene* scene_;

protected:

  void resizeEvent(QResizeEvent* event);

};

#endif // AZMAINWINDOW_H
