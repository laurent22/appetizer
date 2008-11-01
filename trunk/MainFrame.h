/*
  Copyright (C) 2008 Laurent Cozic. All right reserved.
  Use of this source code is governed by a GNU/GPL license that can be
  found in the LICENSE file.
*/

#ifndef __MainFrame_H
#define __MainFrame_H

#include <wx/wx.h>
#include <wx/timer.h>
#include <wx/log.h>
#include <wx/icon.h>
#include <wx/display.h>
#include "imaging/NineSlicesPainter.h"
#include "bitmap_controls/ImagePanel.h"
#include "bitmap_controls/NineSlicesPanel.h"
#include "bitmap_controls/ImageButton.h"
#include "IconPanel.h"
#include "TypeDefinitions.h"
#include "OptionPanel.h"
#include "ApplicationTrayIcon.h"


enum {
  ID_BUTTON_Arrow,
  ID_TIMER_OpenCloseAnimation
};

class MainFrame: public wxFrame {

  private:

    struct WindowDragDataStruct {
      bool DraggingStarted;
      bool Resizing;
      wxPoint InitWindowPos;
      wxPoint InitMousePos;
      wxSize InitWindowSize;
    };
    
    bool rotated_;
    bool needLayoutUpdate_;
    bool needMaskUpdate_;
    bool optionPanelOpen_;
    NineSlicesPanel* backgroundPanel_;
    IconPanel* iconPanel_;
    NineSlicesPainter maskNineSlices_;
    WindowDragDataStruct windowDragData_;
    ImagePanel* resizerPanel_;
    wxLogWindow* logWindow_;
    ImageButton* arrowButton_;
    wxBitmap* arrowButtonCloseIcon_;
    wxBitmap* arrowButtonOpenIcon_;
    OptionPanel* optionPanel_;    
    int optionPanelOpenWidth_;
    int optionPanelMaxOpenWidth_;
    long openCloseAnimationStartTime_;
    long openCloseAnimationDuration_;
    bool openCloseAnimationDockLeft_;
    int openCloseAnimationWindowRight_;
    wxIcon frameIcon_;
    ApplicationTrayIcon taskBarIcon_;

    void UpdateMask();
    void UpdateLayout();
    void UpdateLayout(int width, int height);

  public:

    MainFrame();
    ~MainFrame();

    IconPanel* GetIconPanel();
    OptionPanel* GetOptionPanel();

    int GetMinHeight();
    int GetMinWidth();
    int GetMaxHeight();
    int GetMaxWidth();

    void ConvertToWindowValidCoordinates(const wxDisplay* display, int& x, int& y, int& width, int& height);

    wxMenu* GetContextMenu();

    int GetOptionPanelTotalWidth();

    void Localize();

    void OpenOptionPanel(bool open = true);
    void CloseOptionPanel();
    void ToggleOptionPanel();
    void InvalidateLayout();
    void InvalidateMask();

    void SetRotated(bool rotated);

    int GetDisplayIndex();
    bool IsLeftOfDisplay();
    bool IsTopOfDisplay();

    void OnPaint(wxPaintEvent& evt);
    void OnMouseDown(wxMouseEvent& evt);
    void OnMouseUp(wxMouseEvent& evt);
    void OnMouseMove(wxMouseEvent& evt);
    void OnResizerMouseDown(wxMouseEvent& evt);
    void OnResizerMouseUp(wxMouseEvent& evt);
    void OnResizerMouseMove(wxMouseEvent& evt);
    void OnSize(wxSizeEvent& evt);
    void OnMove(wxMoveEvent& evt);
    void OnEraseBackground(wxEraseEvent &evt);  
    void OnClose(wxCloseEvent& evt);
    void OnImageButtonClick(wxCommandEvent& evt);

  DECLARE_EVENT_TABLE()

};

#endif