import asyncio

async def factorial(n):
    if n == 0 or n == 1:
        return 1
    else:
        # Simulate some asynchronous work
        await asyncio.sleep(0.1)
        return n * await factorial(n - 1)

async def main():
    numbers = [5, 7, 10]
    tasks = [factorial(num) for num in numbers]
    results = await asyncio.gather(*tasks)
    
    for num, result in zip(numbers, results):
        print(f"Factorial of {num} is {result}")

if __name__ == "__main__":
    asyncio.run(main())
