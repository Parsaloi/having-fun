import asyncio


class Node:
    def __init__(self, name, inventory):
        self.name = name
        self.inventory = inventory  # Dictionary representing inventory items
        self.master = None

    def set_master(self, master):
        self.master = master

    async def transfer_data(self, data):
        # Simulate data transfer with a short delay
        await asyncio.sleep(0.1)
        self.master.data = data
        print(f"{self.name} -> Master: Validated data transfer (inventory update from {self.name})")

    # Distributed transfer with Master validation
    def update_inventory(self, data, destination_node):
        # Update local inventory (decrement at source)
        for item, quantity in data.items():
            self.inventory[item] = max(0, self.inventory.get(item, 0) - quantity)

        # Update destination inventory (increment at destination)
        for item, quantity in data.items():
            destination_node.inventory[item] = destination_node.inventory.get(item, 0) + quantity

    def has_sufficient_stock(self, data):
        # Check if source node has enough stock for requested transfer
        for item, quantity in data.items():
            if self.inventory.get(item, 0) < quantity:
                return False
        return True


class CentralInventory:
    def __init__(self):
        self.inventory = {}  # Dictionary to store total inventory for each item

    def add_stock(self, item, quantity):
        self.inventory[item] = self.inventory.get(item, 0) + quantity

    def remove_stock(self, item, quantity):
        self.inventory[item] = max(0, self.inventory.get(item, 0) - quantity)

    def get_total_stock(self, item):
        return self.inventory.get(item, 0)

initial_data = {"Item A": 30, "Item B": 20, "Item C": 15}

async def main():
    # Create master node and pre-configured supporting nodes with initial inventory
    master = Node("Master", {})  # Master tracks total inventory (initially empty)
    node1 = Node("Node 1", {"Item A": 10, "Item B": 5}) #
    node2 = Node("Node 2", {"Item B": 15, "Item C": 3}) #
    node3 = Node("Node 3", {"Item A": 20, "Item C": 12}) #

    # Set master node for supporting nodes (assuming pre-configured)
    node1.set_master(master)
    node2.set_master(master)
    node3.set_master(master)

    # Create CentralInventory object
    central_inventory = CentralInventory()

    # Distribute initial inventory from master to nodes
    for node in [node1, node2, node3]:
        await node.transfer_data(initial_data.copy())  # Transfer data to each node
    # Update central inventory and master inventory (becomes 0)
    for item, quantity in initial_data.items():
        central_inventory.add_stock(item, quantity)
        master.data[item] = 0  # Set master inventory to 0 after distribution

    print("\nInitial inventory distribution:")
    print(f"Central Inventory: {central_inventory.inventory}")
    for node in [master, node1, node2, node3]:
        print(f"{node.name}: {node.inventory}")

    # User interaction: choose nodes to transfer data (inventory updates) between
    while True:
        print("\nInventory Transfer Menu:")
        print(f"1. Transfer items from {node1.name} to {node2.name}")
        print(f"2. Transfer items from {node1.name} to {node3.name}")
        print(f"3. Transfer items from {node2.name} to {node1.name}")
        print(f"4. Transfer items from {node2.name} to {node3.name}")
        print(f"5. Transfer items from {node3.name} to {node1.name}")
        print(f"6. Transfer items from {node3.name} to {node2.name}")
        print("7. Exit")

        choice = input("Enter your choice (1-7): ")

        if choice == "7":
            break

        # Get source and destination nodes
        source_node, destination_node = None, None
        if choice in ("1", "2"):
            source_node = node1
        elif choice in ("3", "4"):
            source_node = node2
        elif choice in ("5", "6"):
            source_node = node3

        if choice in ("1", "6"):
            destination_node = node2
        elif choice in ("2", "4"):
            destination_node = node3
        elif choice in ("3", "5"):
            destination_node = node1
        else:
            print("Invalid choice. Please enter a number between 1 and 7.")
            continue

        # Validate destination node based on source selection
        valid_destinations = {
            node1: [node2, node3],
            node2: [node1, node3],
            node3: [node1, node2],
        }
        if destination_node not in valid_destinations[source_node]:
            print(f"Invalid transfer selection. {source_node.name} cannot transfer to {destination_node.name}.")
            continue

        # Check if source node has sufficient stock
        data_to_transfer = {}
        while True:
            print("\nCentral Inventory (before transfer):")
            print(f"{central_inventory.inventory}")
            print(f"\nInventory before transfer:")
            print(f"{source_node.name}: {source_node.inventory}")
            print(f"{destination_node.name}: {destination_node.inventory}")

            item_name = input("Enter item name to transfer from {} (or 'done'): ".format(source_node.name))
            if item_name == "done":
                if not data_to_transfer:
                    print("No items selected for transfer.")
                break

            quantity = int(input("Enter quantity of {} to transfer: ".format(item_name)))

            # Check for valid item and quantity
            if item_name not in source_node.inventory or quantity <= 0:
                print("Invalid item or quantity. Please try again.")
                continue

            # Check for sufficient stock
            if source_node.inventory[item_name] < quantity:
                print(f"Insufficient stock of {item_name} at {source_node.name}.")
                continue

            data_to_transfer[item_name] = quantity

        # Perform data transfer and update inventory
        if data_to_transfer:
            await source_node.transfer_data(data_to_transfer.copy())  # Avoid modifying original data
            source_node.update_inventory(data_to_transfer, destination_node)
            # Update central inventory
            for item, quantity in data_to_transfer.items():
                central_inventory.remove_stock(item, quantity)  # Decrement from source
                central_inventory.add_stock(item, quantity)    # Increment at destination

            print(f"\nCentral Inventory (after transfer):")
            print(f"{central_inventory.inventory}")
            print(f"\nInventory after transfer:")
            for node in [source_node, destination_node]:
                print(f"{node.name}: {node.inventory}")

asyncio.run(main())
