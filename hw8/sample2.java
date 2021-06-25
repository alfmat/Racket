public class sample2 {
    public static void main(String args[]) {
        SoftwareEngineer Mary = new SoftwareEngineer("Mary", 100000);
        BusinessAnalyst Tom = new BusinessAnalyst("Tom", 80000);
        System.out.println("Mary and Tom start working.");
        System.out.println("\nLet's give them raises!");
    }
}

class Employee {
    String name;
    int salary;

    public Employee(String name, int sal) {
        this.name = name;
        this.salary = sal;
    }

    public void giveRaise(int amount) {
        this.salary += amount;
    }

    public void work() {
        System.out.println("I am working!");
    }
}

class SoftwareEngineer extends Employee {
    public SoftwareEngineer(String name, int sal) {
        super(name, sal);
    }
}

class BusinessAnalyst extends Employee {
    public BusinessAnalyst(String name, int sal) {
        super(name, sal);
    }
}

interface Employee {
    public void giveRaise(int amount);

    public void work();
}