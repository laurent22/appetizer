package events {	

/**
 * Dispatched when a dock item context menu is about to be displayed
 */

public class DockItemMenuOpening extends Event {
	
	/**
	 * The menu that is about to be displayed. You can append
	 * additional items to it.
	 */
	public var menu:Menu;
	
	
	/**
	 * The dock item.
	 */
	public var dockItem:DockItem;
}
}