/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MainWindow.h>
using namespace appetizer;

MainWindow::MainWindow() {
  scene_ = new MainScene();
  setScene(scene_);
}


void MainWindow::resizeEvent(QResizeEvent* event) {
  QGraphicsView::resizeEvent(event);

  scene_->setSceneRect(0, 0, event->size().width(), event->size().height());
  scene_->invalidate();
}