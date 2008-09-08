#include "ImagePanel.h"
#include "wx/dcbuffer.h"


BEGIN_EVENT_TABLE(ImagePanel, wxPanel)
  EVT_PAINT(ImagePanel::OnPaint)
  EVT_ERASE_BACKGROUND(ImagePanel::OnEraseBackground)
END_EVENT_TABLE()


ImagePanel::ImagePanel(wxWindow *owner, int id, wxPoint point, wxSize size):
wxPanel(owner, id, point, size, 0 | wxFRAME_SHAPED | wxSIMPLE_BORDER | wxTRANSPARENT_WINDOW) {
  SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}


void ImagePanel::LoadImage(const wxString& filePath) {
  if (pFilePath == filePath) return;

  pFilePath = filePath;
  pBitmap = wxBitmap(filePath, wxBITMAP_TYPE_PNG);
}


void ImagePanel::OnEraseBackground(wxEraseEvent &evt) {

}


void ImagePanel::FitToContent() {
  SetSize(pBitmap.GetWidth(), pBitmap.GetHeight());
}


void ImagePanel::OnPaint(wxPaintEvent& evt) {
  wxBufferedPaintDC dc(this);  



  //SetBackgroundColour(wxTRANSPARENT_BRUSH->GetColour());

  dc.SetBackgroundMode(wxTRANSPARENT);
  //dc.SetBackground(*wxTRANSPARENT_BRUSH);

  // TODO: The MemoryDC should be cached
	wxMemoryDC sourceDC;
	sourceDC.SelectObjectAsSource(pBitmap);	

  dc.Blit(0, 0, GetClientRect().GetWidth(), GetClientRect().GetHeight(), &sourceDC, 0, 0);

  sourceDC.SelectObjectAsSource(wxNullBitmap);	
}