package scife.util;

import java.lang.ref.*;
import java.util.*;

/**
 * A simple utility class that can verify that an object has been successfully
 * garbage collected.
 */
public class MemoryLeakTester<X> {
	static final int MAX_GC_ITERATIONS = 10;
	static final int GC_SLEEP_TIME = 100;

	private final LinkedList<WeakReference<X>> references = new LinkedList<WeakReference<X>>();

	public MemoryLeakTester() {
	}

	public MemoryLeakTester(X object) {
		this.references.add(new WeakReference<X>(object));
	}

	public void add(X object) {
		this.references.add(new WeakReference<X>(object));
	}

	public int countCollected() {
		int count = 0;
		for (WeakReference<X> ref : references)
			if (ref.get() == null)
				count++;
		return count;
	}

	/**
	 * Attempts to perform a full garbage collection so that all weak references
	 * will be removed. Usually only a single GC is required, but there have been
	 * situations where some unused memory is not cleared up on the first pass.
	 * This method performs a full garbage collection and then validates that the
	 * weak reference now has been cleared. If it hasn't then the thread will
	 * sleep for 50 milliseconds and then retry up to 10 more times. If after this
	 * the object still has not been collected then the assertion will fail.
	 * 
	 * Based upon the method described in:
	 * http://www.javaworld.com/javaworld/javatips/jw-javatip130.html
	 */
	public int collect() {
		Runtime runtime = Runtime.getRuntime();

		runtime.runFinalization();
		runtime.gc();
		for (int i = 1; i < MAX_GC_ITERATIONS; i++) {
			if (countCollected() == references.size())
				break;
			runtime.runFinalization();
			runtime.gc();

			// Pause for a while and then go back around the loop to try again...
			try {
				Thread.sleep(GC_SLEEP_TIME);
			} catch (InterruptedException e) {
				// Ignore any interrupts and just try again...
			}
		}

		return countCollected();
	}

	public boolean isCollected() {
		return collect() == references.size();
	}
}