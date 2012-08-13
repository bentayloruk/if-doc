/******************************************************************************

Copyright (C) 2011 IntelliFactory

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

******************************************************************************/

YAHOO.util.Event.onDOMReady(function () {

    var Y = YAHOO;
    var Dom = Y.util.Dom;
    var layout = new Y.widget.Layout({
        units: [{ position: "left", body: "if-doc-left", width: "300px", scroll: true },
                { position: "center", body: "if-doc-center", scroll: true}]
    });

    var tabView = new Y.widget.TabView("if-doc-tabs");
    var treeView = new Y.widget.TreeView("if-doc-toc");
    var tabs = {};

    function addTriggers(node) {
        var triggers = Dom.getElementsByClassName("if-doc-trigger", "a", node);
        for (var i in triggers) {
            var id = Dom.getAttribute(triggers[i], "href").substring(1);
            if (document.getElementById(id) == null) {
                triggers[i].href = "#";
            } else {
                Y.util.Event.addListener(triggers[i], "click", function (e) {
                    e.preventDefault();
                    var id = Dom.getAttribute(this, "href").substring(1);
                    selectTab(id);
                });
            }
        }
    }

    function selectTab(id) {
        if (tabs[id]) {
            tabView.selectTab(tabView.getTabIndex(tabs[id]));
        } else {
            var element = document.getElementById(id);
            if (element) {
                var body = element.cloneNode(true);
                var title = Dom.getAttribute(element, "data-title");
                var labelEl = document.getElementById("if-doc-tab").cloneNode(true);
                labelEl.id = "tab:" + id;
                var titleEl = Dom.getElementsByClassName("if-doc-title", "span", labelEl)[0];
                var closeEl = Dom.getElementsByClassName("if-doc-close", "span", labelEl)[0];
                titleEl.appendChild(document.createTextNode(title));
                var tab = new Y.widget.Tab({
                    labelEl: labelEl,
                    contentEl: body,
                    active: true
                });
                tabView.addTab(tab);
                tabs[id] = tab;
                Y.util.Event.addListener(closeEl, "click", function () {
                    tabView.removeTab(tab);
                    delete tabs[id];
                });
                addTriggers(body);
            }
        }
    }

    treeView.subscribe("clickEvent", function (x) {
        x.node.toggle();
        var c = x.node.getContentEl();
        var r = Dom.getElementsByClassName("if-doc-trigger", "a", c);
        if (r.length > 0) {
            var id = Dom.getAttribute(r[0], "href").substring(1);
            selectTab(id);
        }
    });

    layout.render();
    treeView.render();
});
