package com.example.socialhub;

import java.util.List;

public class NetworkAndHubs {
    private final SocialHubNetwork network;
    private final List<SocialHubNetwork.Hub> hubs;

    public NetworkAndHubs(SocialHubNetwork network, List<SocialHubNetwork.Hub> hubs) {
        this.network = network;
        this.hubs = hubs;
    }

    public SocialHubNetwork getNetwork() {
        return network;
    }

    public List<SocialHubNetwork.Hub> getHubs() {
        return hubs;
    }
}
