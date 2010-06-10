/*
  Copyright (C) 2008-2010 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#include <stable.h>

#ifndef Appetizer_ScrollPane_H
#define Appetizer_ScrollPane_H

#include <GraphicsItem.h>
#include <GraphicsWindow.h>
#include <ScrollBar.h>

namespace appetizer {

class ScrollPane : public GraphicsItem {

  Q_OBJECT

public:

  ScrollPane(GraphicsWindow* parentWindow);
  GraphicsItem* content();
  void setContent(GraphicsItem* content);
  int contentWidth();
  int contentHeight();
  
 protected:
 
 	void updateDisplay();
  
protected slots:

  void scrollBar_valueChanged();

private:
	
	ScrollBar* scrollBar_;
  GraphicsItem* content_;

  static const int scrollBarGap_ = 2;

};

}
#endif // Appetizer_ScrollPane_H
