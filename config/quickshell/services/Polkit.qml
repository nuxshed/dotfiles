pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Polkit

Singleton {
    id: root

    readonly property bool registered: agent.isRegistered
    readonly property AuthFlow flow: agent.flow
    property bool asking: false

    function prompt(): void {
        const f = root.flow;
        if (root.asking || !f || !f.isResponseRequired || f.isCompleted)
            return;
        const label = f.inputPrompt.trim().replace(/:$/, "");
        root.asking = true;
        Prompt.ask({
            title: "Authentication required",
            subtitle: f.message.replace(/\.$/, ""),
            placeholder: label || "Password",
            action: "Authenticate",
            error: f.failed ? (f.supplementaryIsError && f.supplementaryMessage ? f.supplementaryMessage : "Wrong password, try again") : "",
            echo: f.responseVisible,
            onSubmit: value => {
                root.asking = false;
                if (root.flow === f)
                    f.submit(value);
            },
            onCancel: () => {
                root.asking = false;
                if (root.flow === f && !f.isCompleted)
                    f.cancelAuthenticationRequest();
            }
        });
    }

    function cancel(): void {
        if (root.asking)
            Prompt.close();
        else if (root.flow)
            root.flow.cancelAuthenticationRequest();
    }

    PolkitAgent {
        id: agent

        onFlowChanged: {
            if (agent.flow)
                root.prompt();
            else if (root.asking)
                Prompt.close();
        }
    }

    Connections {
        target: root.flow

        function onIsResponseRequiredChanged(): void {
            root.prompt();
        }

        function onIsCompletedChanged(): void {
            if (root.flow.isCompleted && root.asking)
                Prompt.close();
        }
    }
}
