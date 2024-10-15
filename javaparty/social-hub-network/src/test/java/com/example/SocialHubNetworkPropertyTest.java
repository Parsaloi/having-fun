package com.example.socialhub;

import net.jqwik.api.*;
import net.jqwik.api.arbitraries.IntegerArbitrary;
import net.jqwik.api.constraints.Size;

import java.util.*;
import java.util.stream.*;

import static org.junit.jupiter.api.Assertions.*;

class SocialHubNetworkPropertyTest {

    @Provide
    Arbitrary<String> hubNames() {
        return Arbitraries.strings().alpha().ofMinLength(3).ofMaxLength(10);
    }

    @Provide
    Arbitrary<Integer> travelTimes() {
        return Arbitraries.integers().between(1, 100);
    }

    @Property
    void pathIsValid(@ForAll("connectedNetworkGenerator") NetworkAndHubs networkAndHubs) {
        SocialHubNetwork network = networkAndHubs.getNetwork();
        List<SocialHubNetwork.Hub> hubs = networkAndHubs.getHubs();
        Assume.that(hubs.size() >= 2);

        SocialHubNetwork.Hub start = hubs.get(0);
        SocialHubNetwork.Hub end = hubs.get(hubs.size() - 1);

        SocialHubNetwork.Path path = network.findFastestPath(start, end);

        assertNotNull(path, "Path should not be null");
        assertFalse(path.hubs().isEmpty(), "Path should not be empty");
        assertEquals(start, path.hubs().get(0), "Path should start at the start hub");
        assertEquals(end, path.hubs().get(path.hubs().size() - 1), "Path should end at the end hub");
        assertTrue(path.totalTime() >= 0, "Total time should be non-negative");

        // Check if the path is continuous
        IntStream.range(0, path.hubs().size() - 1)
                 .forEach(i -> assertTrue(network.areConnected(path.hubs().get(i), path.hubs().get(i + 1)),
                                          "Path should be continuous"));
    }

    @Property
    void shortestPathIsOptimal(@ForAll("connectedNetworkGenerator") NetworkAndHubs networkAndHubs) {
        SocialHubNetwork network = networkAndHubs.getNetwork();
        List<SocialHubNetwork.Hub> hubs = networkAndHubs.getHubs();
        Assume.that(hubs.size() >= 2);

        SocialHubNetwork.Hub start = hubs.get(0);
        SocialHubNetwork.Hub end = hubs.get(hubs.size() - 1);

        SocialHubNetwork.Path shortestPath = network.findFastestPath(start, end);

        // Check all possible paths
        List<List<SocialHubNetwork.Hub>> allPaths = findAllPaths(network, start, end);
        allPaths.stream()
                .map(path -> calculatePathTime(network, path))
                .forEach(pathTime -> assertTrue(pathTime >= shortestPath.totalTime(),
                                                "No path should be shorter than the shortest path"));
    }

    @Provide
    Arbitrary<NetworkAndHubs> connectedNetworkGenerator() {
        return Combinators.combine(
            Arbitraries.integers().between(2, 10),
            hubNames(),
            travelTimes()
        ).as((numHubs, hubName, travelTime) -> {
            SocialHubNetwork network = new SocialHubNetwork();
            List<SocialHubNetwork.Hub> hubs = IntStream.range(0, numHubs)
                .mapToObj(i -> new SocialHubNetwork.Hub(hubName + i))
                .peek(hub -> network.applyOperation(new SocialHubNetwork.AddHub(hub)))
                .toList();

            // Ensure the network is connected
            for (int i = 1; i < hubs.size(); i++) {
                network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(hubs.get(i-1), hubs.get(i), travelTime)));
            }

            // Add some random additional connections
            hubs.stream()
                .flatMap(hub1 -> hubs.stream()
                    .filter(hub2 -> !hub1.equals(hub2))
                    .filter(hub2 -> !network.areConnected(hub1, hub2))
                    .filter(hub2 -> Arbitraries.integers().between(0, 2).sample() > 0) // 2/3 probability
                    .map(hub2 -> new SocialHubNetwork.Road(hub1, hub2, travelTime)))
                .forEach(road -> network.applyOperation(new SocialHubNetwork.AddRoad(road)));

            return new NetworkAndHubs(network, hubs);
        });
    }

    @Provide
    Arbitrary<SocialHubNetwork.Hub> hubGenerator() {
        return hubNames().map(SocialHubNetwork.Hub::new);
    }

    private List<List<SocialHubNetwork.Hub>> findAllPaths(SocialHubNetwork network, SocialHubNetwork.Hub start, SocialHubNetwork.Hub end) {
        List<List<SocialHubNetwork.Hub>> allPaths = new ArrayList<>();
        dfs(network, start, end, new HashSet<>(), new ArrayList<>(), allPaths);
        return allPaths;
    }

    private void dfs(SocialHubNetwork network, SocialHubNetwork.Hub current, SocialHubNetwork.Hub end,
                     Set<SocialHubNetwork.Hub> visited, List<SocialHubNetwork.Hub> currentPath,
                     List<List<SocialHubNetwork.Hub>> allPaths) {
        visited.add(current);
        currentPath.add(current);

        if (current.equals(end)) {
            allPaths.add(new ArrayList<>(currentPath));
        } else {
            network.getNeighbors(current).stream()
                .filter(neighbor -> !visited.contains(neighbor))
                .forEach(neighbor -> dfs(network, neighbor, end, visited, currentPath, allPaths));
        }

        visited.remove(current);
        currentPath.remove(currentPath.size() - 1);
    }

    private int calculatePathTime(SocialHubNetwork network, List<SocialHubNetwork.Hub> path) {
        return IntStream.range(0, path.size() - 1)
            .map(i -> network.getTravelTime(path.get(i), path.get(i + 1)))
            .sum();
    }

    @Example
    void testEmptyNetwork() {
        SocialHubNetwork network = new SocialHubNetwork();
        SocialHubNetwork.Hub start = new SocialHubNetwork.Hub("Start");
        SocialHubNetwork.Hub end = new SocialHubNetwork.Hub("End");

        SocialHubNetwork.Path path = network.findFastestPath(start, end);
        assertEquals(-1, path.totalTime(), "Empty network should return no path");
        assertTrue(path.hubs().isEmpty(), "Empty network should return empty path");
    }

    @Example
    void testSingleHub() {
        SocialHubNetwork network = new SocialHubNetwork();
        SocialHubNetwork.Hub hub = new SocialHubNetwork.Hub("SingleHub");
        network.applyOperation(new SocialHubNetwork.AddHub(hub));

        SocialHubNetwork.Path path = network.findFastestPath(hub, hub);
        assertEquals(0, path.totalTime(), "Path to same hub should have zero time");
        assertEquals(List.of(hub), path.hubs(), "Path to same hub should contain only that hub");
    }

    @Example
    void testTwoConnectedHubs() {
        SocialHubNetwork network = new SocialHubNetwork();
        SocialHubNetwork.Hub hub1 = new SocialHubNetwork.Hub("Hub1");
        SocialHubNetwork.Hub hub2 = new SocialHubNetwork.Hub("Hub2");
        network.applyOperation(new SocialHubNetwork.AddHub(hub1));
        network.applyOperation(new SocialHubNetwork.AddHub(hub2));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(hub1, hub2, 10)));

        SocialHubNetwork.Path path = network.findFastestPath(hub1, hub2);
        assertEquals(10, path.totalTime(), "Path between connected hubs should have correct time");
        assertEquals(List.of(hub1, hub2), path.hubs(), "Path should contain both hubs in correct order");
    }

    @Example
    void testNetworkWithMultiplePaths() {
        SocialHubNetwork network = new SocialHubNetwork();
        SocialHubNetwork.Hub a = new SocialHubNetwork.Hub("A");
        SocialHubNetwork.Hub b = new SocialHubNetwork.Hub("B");
        SocialHubNetwork.Hub c = new SocialHubNetwork.Hub("C");
        SocialHubNetwork.Hub d = new SocialHubNetwork.Hub("D");

        network.applyOperation(new SocialHubNetwork.AddHub(a));
        network.applyOperation(new SocialHubNetwork.AddHub(b));
        network.applyOperation(new SocialHubNetwork.AddHub(c));
        network.applyOperation(new SocialHubNetwork.AddHub(d));

        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(a, b, 1)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(b, d, 2)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(a, c, 2)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(c, d, 1)));

        SocialHubNetwork.Path path = network.findFastestPath(a, d);
        assertEquals(3, path.totalTime(), "Shortest path should have total time of 3");
        assertEquals(List.of(a, c, d), path.hubs(), "Shortest path should be A -> C -> D");
    }

    @Example
    void testNetworkWithCycle() {
        SocialHubNetwork network = new SocialHubNetwork();
        SocialHubNetwork.Hub a = new SocialHubNetwork.Hub("A");
        SocialHubNetwork.Hub b = new SocialHubNetwork.Hub("B");
        SocialHubNetwork.Hub c = new SocialHubNetwork.Hub("C");

        network.applyOperation(new SocialHubNetwork.AddHub(a));
        network.applyOperation(new SocialHubNetwork.AddHub(b));
        network.applyOperation(new SocialHubNetwork.AddHub(c));

        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(a, b, 1)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(b, c, 2)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(c, a, 3)));

        SocialHubNetwork.Path path = network.findFastestPath(a, c);
        assertEquals(3, path.totalTime(), "Shortest path should have total time of 3");
        assertEquals(List.of(a, b, c), path.hubs(), "Shortest path should be A -> B -> C");
    }

    @Example
    void testNetworkWithVaryingTravelTimes() {
        SocialHubNetwork network = new SocialHubNetwork();
        SocialHubNetwork.Hub a = new SocialHubNetwork.Hub("A");
        SocialHubNetwork.Hub b = new SocialHubNetwork.Hub("B");
        SocialHubNetwork.Hub c = new SocialHubNetwork.Hub("C");
        SocialHubNetwork.Hub d = new SocialHubNetwork.Hub("D");

        network.applyOperation(new SocialHubNetwork.AddHub(a));
        network.applyOperation(new SocialHubNetwork.AddHub(b));
        network.applyOperation(new SocialHubNetwork.AddHub(c));
        network.applyOperation(new SocialHubNetwork.AddHub(d));

        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(a, b, 10)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(b, c, 1)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(c, d, 1)));
        network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(a, d, 15)));

        SocialHubNetwork.Path path = network.findFastestPath(a, d);
        assertEquals(12, path.totalTime(), "Shortest path should have total time of 12");
        assertEquals(List.of(a, b, c, d), path.hubs(), "Shortest path should be A -> B -> C -> D");
    }

    @Property
    void testLargeNetwork(@ForAll("largeNetworkGenerator") NetworkAndHubs networkAndHubs) {
        SocialHubNetwork network = networkAndHubs.getNetwork();
        List<SocialHubNetwork.Hub> hubs = networkAndHubs.getHubs();
        Assume.that(hubs.size() >= 2);

        SocialHubNetwork.Hub start = hubs.get(0);
        SocialHubNetwork.Hub end = hubs.get(hubs.size() - 1);

        SocialHubNetwork.Path path = network.findFastestPath(start, end);

        assertNotNull(path, "Path should not be null");
        assertFalse(path.hubs().isEmpty(), "Path should not be empty");
        assertEquals(start, path.hubs().get(0), "Path should start at the start hub");
        assertEquals(end, path.hubs().get(path.hubs().size() - 1), "Path should end at the end hub");
        assertTrue(path.totalTime() >= 0, "Total time should be non-negative");
    }

    @Provide
    Arbitrary<NetworkAndHubs> largeNetworkGenerator() {
        return Combinators.combine(
            Arbitraries.integers().between(50, 100),
            hubNames(),
            travelTimes()
        ).as((numHubs, hubName, travelTime) -> {
            SocialHubNetwork network = new SocialHubNetwork();
            List<SocialHubNetwork.Hub> hubs = IntStream.range(0, numHubs)
                .mapToObj(i -> new SocialHubNetwork.Hub(hubName + i))
                .peek(hub -> network.applyOperation(new SocialHubNetwork.AddHub(hub)))
                .toList();

            // Ensure the network is connected
            for (int i = 1; i < hubs.size(); i++) {
                network.applyOperation(new SocialHubNetwork.AddRoad(new SocialHubNetwork.Road(hubs.get(i-1), hubs.get(i), travelTime)));
            }

            // Add some random additional connections
            hubs.stream()
                .flatMap(hub1 -> hubs.stream()
                    .filter(hub2 -> !hub1.equals(hub2))
                    .filter(hub2 -> !network.areConnected(hub1, hub2))
                    .filter(hub2 -> Arbitraries.integers().between(0, 5).sample() == 0) // 1/6 probability
                    .map(hub2 -> new SocialHubNetwork.Road(hub1, hub2, Arbitraries.integers().between(1, 100).sample())))
                .forEach(road -> network.applyOperation(new SocialHubNetwork.AddRoad(road)));

            return new NetworkAndHubs(network, hubs);
        });
    }
}
