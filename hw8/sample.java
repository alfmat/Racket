class Sample {
    public static void main(String args[]) {
        // Pizza object inherits the constructor from Food
        Pizza pepperoni = new Pizza();
        pepperoni.speak();
        // polymorphism of a Pizza object to a Food object
        Food food_item = pepperoni;
        System.out.println(food_item);
    }
}

class Food {
    public Food() {
        System.out.println("making food!");
    }

    public String toString() {
        return "I am food";
    }
}

class Pizza extends Food {
    // encapsulation of price data
    int price = 10;

    public void speak() {
        System.out.println("I am a Pizza that costs $" + price);
    }
}
