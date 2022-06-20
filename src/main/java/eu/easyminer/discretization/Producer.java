package eu.easyminer.discretization;

public interface Producer<T> {
    void produce(Consumer<T> consumer);
}
