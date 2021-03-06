/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.imaginea.kodebeagle.ui;

import com.intellij.icons.AllIcons;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationManagerEx;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.ui.Gray;
import com.intellij.ui.components.JBLabel;
import com.intellij.ui.components.JBPanel;
import com.intellij.util.ui.UIUtil;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class LegalNotice extends DialogWrapper {
    private Project project;

    protected static final String KODE_BEAGLE_IDEA_LEGAL_NOTICE = "KodeBeagleIdeaLegalNotice";
    private static final String KODEBEAGLEIDEA = "kodebeagleidea";
    private static final String DECLINE = "Decline";
    private static final String LEGAL_NOTICE_TITLE = "Kode Beagle";
    private static final String ACCEPT = "Accept";
    private static final String LEGAL_NOTICE_MESSAGE =
            String.format("%n  <b>IMPORTANT: BY ACCESSING AND USING KODE BEAGLE IDEA PLUGIN, "
                    + "YOU AGREE TO THE CERTAIN TERMS AND CONDITIONS %n  "
                    + "SET FORTH IN THE END-USER LICENSE AGREEMENT AND QUOTED BELOW. "
                    + "IF YOU DO NOT AGREE WITH THESE TERMS OR CONDITIONS, %n  "
                    + "DO NOT ACCESS OR USE KODE BEAGLE.</b>%n  <br><br> %n%n"
                    + "  The Software includes code crawling functionality (\"Kode Beagle\") <br>"
                    + "that enables searching source code from the open source github projects"
                    + "Licensee acknowledges source code might be protected by copyright and "
                    + "trademark laws. %n Before using Kode Beagle, Licensee should make sure that "
                    + "copying of souce code is not prohibited by the applicable license agreement"
                    + "  (except to the extent that Licensee may be expressly permitted under "
                    + "applicable law) %n   or that Licensee has obtained permission to copy the "
                    + "souce code from the copyright owner.%n%n"
                    + "  Using Kode Beagle is entirely optional. Licensor does neither encourage "
                    + "nor condone %n the use of Kode Beagle, and disclaims any liability for"
                    + "Licensee's use of Kode Beagle %n in violation of applicable laws.%n"
                    + "<br><br><b>Declining/Cancelling this will disable Kode Beagle "
                    + "and restart Idea</b>");
    private static final String DIV_STYLE_MARGIN_5PX = "<div style='margin:5px;'>";
    private static final String DIV = "</div>";
    private static final Dimension MESSAGE_EDITOR_PANE_PREFERRED_SIZE = new Dimension(500, 100);
    private static final BorderLayout LEGAL_NOTICE_LAYOUT = new BorderLayout(10, 0);
    private static boolean legalNoticeAccepted =
            PropertiesComponent.getInstance().getBoolean(KODE_BEAGLE_IDEA_LEGAL_NOTICE,
                    false);


    public static boolean isLegalNoticeAccepted() {
        return legalNoticeAccepted;
    }

    protected final void showLegalNotice() {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                new LegalNotice(project).show();
            }
        });
    }

    protected LegalNotice(@Nullable final Project pProject) {
        super(pProject);
        this.project = pProject;
        setTitle(LEGAL_NOTICE_TITLE);
        setOKButtonText(ACCEPT);
        init();
        pack();
    }

    @Nullable
    @Override
    protected final JComponent createCenterPanel() {

        JPanel iconPanel = new JBPanel(new BorderLayout());
        iconPanel.add(new JBLabel(AllIcons.General.WarningDialog), BorderLayout.NORTH);

        JEditorPane messageEditorPane = new JEditorPane();
        messageEditorPane.setEditorKit(UIUtil.getHTMLEditorKit());
        messageEditorPane.setEditable(false);
        messageEditorPane.setPreferredSize(MESSAGE_EDITOR_PANE_PREFERRED_SIZE);
        messageEditorPane.setBorder(BorderFactory.createLineBorder(Gray._200));
        String text = DIV_STYLE_MARGIN_5PX + LEGAL_NOTICE_MESSAGE + DIV;
        messageEditorPane.setText(text);

        JPanel legalNoticePanel = new JPanel(LEGAL_NOTICE_LAYOUT);
        legalNoticePanel.add(iconPanel, BorderLayout.WEST);
        legalNoticePanel.add(messageEditorPane, BorderLayout.CENTER);
        return legalNoticePanel;
    }

    @NotNull
    @Override
    protected final Action[] createActions() {
        DialogWrapperAction declineAction = new DialogWrapperAction(DECLINE) {
            @Override
            protected final void doAction(final ActionEvent e) {
                PluginManagerCore.disablePlugin(KODEBEAGLEIDEA);
                ApplicationManagerEx.getApplicationEx().restart(true);
            }
        };
        return new Action[]{getOKAction(), declineAction, getCancelAction()};
    }

    @Override
    protected final void doOKAction() {
        super.doOKAction();
        PropertiesComponent.getInstance().setValue(KODE_BEAGLE_IDEA_LEGAL_NOTICE,
                Boolean.TRUE.toString());
    }

    @Override
    public final void doCancelAction() {
        super.doCancelAction();
        PluginManagerCore.disablePlugin(KODEBEAGLEIDEA);
        ApplicationManagerEx.getApplicationEx().restart(true);
    }
}
