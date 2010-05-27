/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>
#include <MainWindow.h>
using namespace appetizer;

MainWindow::MainWindow()/*: QWidget(NULL, Qt::FramelessWindowHint)*/ {
  view_ = new QGraphicsView(this);
  
  scene_ = new MainScene();
  view_->setScene(scene_);
  view_->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  view_->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

  
  //QPixmap p("c:\\Users\\Laurent_2\\Desktop\\MaskTest.png");
  //QPainter painter(&p);
  //QBitmap b = p.mask();
  //setMask(b);

  view_->show();
}


void MainWindow::resizeEvent(QResizeEvent* event) {
  QWidget::resizeEvent(event);

  view_->resize(width(), height());

  scene_->setSceneRect(0, 0, event->size().width(), event->size().height());
  scene_->invalidate();

  //QBitmap* sceneMask = scene_->composedMask();
  //QPainter painter(this);
  //painter.drawImage(QRect(0,0,width(),height()), sceneMask->toImage(), QRect(0,0,sceneMask->width(), sceneMask->height()));
  //setMask(*sceneMask);
}