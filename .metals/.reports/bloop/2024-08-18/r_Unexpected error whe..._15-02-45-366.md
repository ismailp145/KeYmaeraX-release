error id: lrYGCO/dqOMsrWvlRhvb1A==
### Bloop error:

Unexpected error when copying <WORKSPACE>/.bloop/core/bloop-internal-classes/classes-Metals-V-rNB1mmT82mH3NKmwMNWQ==-FkM9o4i3RfC8i9kFWeChBA==/META-INF/semanticdb/keymaerax-core/src/main/scala/edu/cmu/cs/ls/keymaerax/Configuration.scala.semanticdb to <WORKSPACE>/.bloop/core/bloop-bsp-clients-classes/classes-Metals-GfRJUbgwSUyxSgphdLcmoQ==/META-INF/semanticdb/keymaerax-core/src/main/scala/edu/cmu/cs/ls/keymaerax/Configuration.scala.semanticdb, you might need to restart the build server.
java.nio.file.NoSuchFileException: <WORKSPACE>/.bloop/core/bloop-internal-classes/classes-Metals-V-rNB1mmT82mH3NKmwMNWQ==-FkM9o4i3RfC8i9kFWeChBA==/META-INF/semanticdb/keymaerax-core/src/main/scala/edu/cmu/cs/ls/keymaerax/Configuration.scala.semanticdb
	at java.base/sun.nio.fs.UnixException.translateToIOException(UnixException.java:92)
	at java.base/sun.nio.fs.UnixException.rethrowAsIOException(UnixException.java:106)
	at java.base/sun.nio.fs.UnixException.rethrowAsIOException(UnixException.java:111)
	at java.base/sun.nio.fs.UnixCopyFile.copy(UnixCopyFile.java:552)
	at java.base/sun.nio.fs.UnixFileSystemProvider.copy(UnixFileSystemProvider.java:257)
	at java.base/java.nio.file.Files.copy(Files.java:1304)
	at bloop.io.ParallelOps$.copy$1(ParallelOps.scala:169)
	at bloop.io.ParallelOps$.$anonfun$copyDirectories$8(ParallelOps.scala:206)
	at scala.runtime.java8.JFunction0$mcV$sp.apply(JFunction0$mcV$sp.java:23)
	at monix.eval.internal.TaskRunLoop$.startFuture(TaskRunLoop.scala:494)
	at monix.eval.Task.runToFutureOpt(Task.scala:586)
	at monix.eval.internal.TaskDeprecated$Extensions.runSyncMaybeOptPrv(TaskDeprecated.scala:128)
	at monix.eval.internal.TaskDeprecated$Extensions.$anonfun$coeval$1(TaskDeprecated.scala:303)
	at monix.eval.Coeval$Always.apply(Coeval.scala:1451)
	at monix.eval.Coeval.value(Coeval.scala:258)
	at bloop.io.ParallelOps$.$anonfun$copyDirectories$7(ParallelOps.scala:234)
	at monix.reactive.internal.consumers.ForeachAsyncConsumer$$anon$1.onNext(ForeachAsyncConsumer.scala:44)
	at monix.reactive.internal.consumers.LoadBalanceConsumer$$anon$1.$anonfun$signalNext$1(LoadBalanceConsumer.scala:218)
	at monix.execution.internal.InterceptRunnable.run(InterceptRunnable.scala:27)
	at java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1144)
	at java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:642)
	at java.base/java.lang.Thread.run(Thread.java:1589)
#### Short summary: 

Unexpected error when copying <WORKSPACE>/.bloop/core/bloop-internal-classes/classes-Metals-V-rNB1mm...