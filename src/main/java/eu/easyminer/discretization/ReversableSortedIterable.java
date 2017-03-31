package eu.easyminer.discretization;

/**
 * Created by propan on 31. 3. 2017.
 */
public interface ReversableSortedIterable<T> extends SortedIterable<T> {

    SortedIterable<T> reverse();

}
