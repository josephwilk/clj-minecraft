export _JAVA_OPTIONS="-Xmx1g"

install:
	@echo "Downloading Spigot Minecraft server..."
	mkdir -p minecraft
	cd minecraft && wget --no-clobber https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar

	@echo "Building Spigot Minecraft server..."
	cd minecraft && java -jar BuildTools.jar
	cp minecraft/Bukkit/target/bukkit-1.8-R0.1-SNAPSHOT.jar resources/

	@echo "Compling clj-minecraft..."
	lein uberjar
	cp target/cljminecraft-1.0.6-SNAPSHOT-standalone.jar minecraft/plugins/

	@echo "Minecraft is ready!" 
