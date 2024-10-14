package com.example.socialhub;

import java.util.*;
import java.util.stream.*;

public class SocialHubNetwork {

    // Records for immutable data structures
    public record Hub(String name) {}
    public record Road(Hub from, Hub to, int travelTime) {}
    public record Path(List<Hub> hubs, int totalTime) {}

    // Sealed interface for graph operations
    public sealed interface GraphOperation permits AddHub, AddRoad {}
    public record AddHub(Hub hub) implements GraphOperation {}
    public record AddRoad(Road road) implements GraphOperation {}

    private final Map<Hub, Map<Hub, Integer>> graph;

    public SocialHubNetwork() {
        this.graph = new HashMap<>();
    }

    public void applyOperation(GraphOperation operation) {
        switch (operation) {
            case AddHub addHub -> graph.putIfAbsent(addHub.hub(), new HashMap<>());
            case AddRoad addRoad -> {
                graph.get(addRoad.road().from()).put(addRoad.road().to(), addRoad.road().travelTime());
                graph.get(addRoad.road().to()).put(addRoad.road().from(), addRoad.road().travelTime());
            }
        }
    }

    public boolean containsHub(Hub hub) {
        return graph.containsKey(hub);
    }

    public boolean areConnected(Hub hub1, Hub hub2) {
        return graph.get(hub1).containsKey(hub2);
    }

    public Set<Hub> getNeighbors(Hub hub) {
        return graph.get(hub).keySet();
    }

    public int getTravelTime(Hub from, Hub to) {
        return graph.get(from).get(to);
    }

    public Path findFastestPath(Hub start, Hub end) {
        if (!containsHub(start) || !containsHub(end)) {
            return new Path(List.of(), -1); // Return invalid path if either hub doesn't exist
        }

        if (start.equals(end)) {
            return new Path(List.of(start), 0); // Return path with zero time if start and end are the same
        }

        record State(Hub hub, int time, List<Hub> path) {}

        Map<Hub, Integer> times = new HashMap<>();
        PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt(State::time));

        queue.offer(new State(start, 0, List.of(start)));
        times.put(start, 0);

        while (!queue.isEmpty()) {
            var current = queue.poll();

            if (current.hub().equals(end)) {
                return new Path(current.path(), current.time());
            }

            if (current.time() > times.getOrDefault(current.hub(), Integer.MAX_VALUE)) {
                continue;
            }

            Map<Hub, Integer> neighbors = graph.get(current.hub());
            if (neighbors != null) {
                neighbors.forEach((neighbor, time) -> {
                    int newTime = current.time() + time;
                    if (newTime < times.getOrDefault(neighbor, Integer.MAX_VALUE)) {
                        times.put(neighbor, newTime);
                        var newPath = Stream.concat(current.path().stream(), Stream.of(neighbor))
                                            .toList();
                        queue.offer(new State(neighbor, newTime, newPath));
                    }
                });
            }
        }

        return new Path(List.of(), -1); // No path found
    }

    // public Path findFastestPath(Hub start, Hub end) {
    //     record State(Hub hub, int time, List<Hub> path) {}
    //
    //     Map<Hub, Integer> times = new HashMap<>();
    //     PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt(State::time));
    //
    //     queue.offer(new State(start, 0, List.of(start)));
    //     times.put(start, 0);
    //
    //     while (!queue.isEmpty()) {
    //         var current = queue.poll();
    //
    //         if (current.hub().equals(end)) {
    //             return new Path(current.path(), current.time());
    //         }
    //
    //         if (current.time() > times.getOrDefault(current.hub(), Integer.MAX_VALUE)) {
    //             continue;
    //         }
    //
    //         graph.get(current.hub()).forEach((neighbor, time) -> {
    //             int newTime = current.time() + time;
    //             if (newTime < times.getOrDefault(neighbor, Integer.MAX_VALUE)) {
    //                 times.put(neighbor, newTime);
    //                 var newPath = Stream.concat(current.path().stream(), Stream.of(neighbor))
    //                                     .toList();
    //                 queue.offer(new State(neighbor, newTime, newPath));
    //             }
    //         });
    //     }
    //
    //     return new Path(List.of(), -1); // No path found
    // }

    // public static void main(String[] args) {
    //     SocialHubNetwork network = new SocialHubNetwork();
    //
    //     var coffeeShop = new Hub("CoffeeShop");
    //     var park = new Hub("Park");
    //     var library = new Hub("Library");
    //     var restaurant = new Hub("Restaurant");
    //     var mall = new Hub("Mall");
    //
    //     List.of(coffeeShop, park, library, restaurant, mall)
    //         .forEach(hub -> network.applyOperation(new AddHub(hub)));
    //
    //     List.of(
    //         new Road(coffeeShop, park, 5),
    //         new Road(coffeeShop, library, 10),
    //         new Road(park, restaurant, 15),
    //         new Road(library, restaurant, 5),
    //         new Road(library, mall, 20),
    //         new Road(restaurant, mall, 10)
    //     ).forEach(road -> network.applyOperation(new AddRoad(road)));
    //
    //     Path fastestPath = network.findFastestPath(coffeeShop, mall);
    //     System.out.println("Fastest path: " + fastestPath.hubs().stream().map(Hub::name).collect(Collectors.joining(" -> ")));
    //     System.out.println("Total travel time: " + fastestPath.totalTime() + " minutes");
    // }
}
