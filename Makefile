export _JAVA_OPTIONS="-Xmx1g"

PLUGIN_JAR="minecraft/plugins/cljminecraft-1.0.6-SNAPSHOT-standalone.jar"

install:
	@echo "Downloading Spigot Minecraft server..."
	mkdir -p minecraft
	mkdir -p minecraft/plugins

	cd minecraft && wget --no-clobber https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar

	@echo "Building Spigot Minecraft server..."
	cd minecraft && java -jar BuildTools.jar
	cp minecraft/Bukkit/target/bukkit-1.8.7-R0.1-SNAPSHOT.jar resources/bukkit-1.8-R0.1-SNAPSHOT.jar

	@echo "Compling clj-minecraft..."
	lein uberjar
	cp target/cljminecraft-1.0.6-SNAPSHOT-standalone.jar minecraft/plugins/

	@echo "Minecraft is ready!"

install-plugin: $(PLUGIN_JAR)

$(PLUGIN_JAR):
	@echo "Compling clj-minecraft..."
	lein uberjar
	cp target/cljminecraft-1.0.6-SNAPSHOT-standalone.jar minecraft/plugins/

reset-world:
	cd minecraft && rm -rf world/*
	cd minecraft && rm -rf world_nether/*
	cd minecraft && rm -rf world_the_end/*
