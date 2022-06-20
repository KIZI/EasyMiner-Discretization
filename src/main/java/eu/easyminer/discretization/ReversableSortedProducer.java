package eu.easyminer.discretization;

/**
 * Created by propan on 31. 3. 2017.
 */
public interface ReversableSortedProducer<T> extends SortedProducer<T> {

    SortedProducer<T> reverse();

}
