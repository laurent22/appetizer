/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_GraphicsWindow_H
#define Appetizer_GraphicsWindow_H


namespace appetizer {
	

class GraphicsWindow : public QWidget {

  Q_OBJECT

public:

  GraphicsWindow();
  virtual QPixmap* getWindowPixmap();
  void invalidateDisplay();

protected:

  void updateDisplay(QPixmap& pixmap);

private:

  int frameRate_;
  int lastUpdateTime_;
  QTimer updateTimer_;
  QTime time_;
  mutable int timeBetweenFrames_;
  int timeBetweenFrames() const;

private slots:

  void updateTimer_timeout();

};

}
#endif // Appetizer_GraphicsWindow_H
