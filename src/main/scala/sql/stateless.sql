-- MySQL dump 10.16  Distrib 10.1.37-MariaDB, for Linux (x86_64)
--
-- Host: localhost    Database: stateless
-- ------------------------------------------------------
-- Server version	10.1.37-MariaDB

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `Address`
--

DROP TABLE IF EXISTS `Address`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Address` (
  `address_id` smallint(6) NOT NULL,
  `street` varchar(30) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`address_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Address`
--

LOCK TABLES `Address` WRITE;
/*!40000 ALTER TABLE `Address` DISABLE KEYS */;
/*!40000 ALTER TABLE `Address` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Alias`
--

DROP TABLE IF EXISTS `Alias`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Alias` (
  `alias_id` smallint(6) DEFAULT NULL,
  `alias` varchar(30) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `person_id` smallint(6) DEFAULT NULL,
  KEY `person_id` (`person_id`),
  CONSTRAINT `Alias_ibfk_1` FOREIGN KEY (`person_id`) REFERENCES `Person` (`person_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Alias`
--

LOCK TABLES `Alias` WRITE;
/*!40000 ALTER TABLE `Alias` DISABLE KEYS */;
INSERT INTO `Alias` VALUES (0,'A',0),(1,'B',1),(2,'A-her',0),(3,'C',2);
/*!40000 ALTER TABLE `Alias` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Couple`
--

DROP TABLE IF EXISTS `Couple`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Couple` (
  `couple_id` smallint(6) NOT NULL,
  `her` smallint(6) DEFAULT NULL,
  `him` smallint(6) DEFAULT NULL,
  PRIMARY KEY (`couple_id`),
  KEY `fk_her` (`her`),
  KEY `fk_him` (`him`),
  CONSTRAINT `fk_her` FOREIGN KEY (`her`) REFERENCES `Person` (`person_id`),
  CONSTRAINT `fk_him` FOREIGN KEY (`him`) REFERENCES `Person` (`person_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Couple`
--

LOCK TABLES `Couple` WRITE;
/*!40000 ALTER TABLE `Couple` DISABLE KEYS */;
INSERT INTO `Couple` VALUES (0,0,1),(1,2,3),(2,4,5);
/*!40000 ALTER TABLE `Couple` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `Person`
--

DROP TABLE IF EXISTS `Person`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `Person` (
  `person_id` smallint(6) NOT NULL,
  `name` varchar(30) COLLATE utf8mb4_unicode_ci DEFAULT NULL,
  `age` smallint(6) DEFAULT NULL,
  `weight` smallint(6) DEFAULT NULL,
  `address` smallint(6) DEFAULT NULL,
  PRIMARY KEY (`person_id`),
  KEY `fk_person_address` (`address`),
  CONSTRAINT `fk_person_address` FOREIGN KEY (`address`) REFERENCES `Address` (`address_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `Person`
--

LOCK TABLES `Person` WRITE;
/*!40000 ALTER TABLE `Person` DISABLE KEYS */;
INSERT INTO `Person` VALUES (0,'Alex',60,60,NULL),(1,'Bert',55,55,NULL),(2,'Cora',33,33,NULL),(3,'Drew',31,31,NULL),(4,'Edna',21,21,NULL),(5,'Fred',60,60,NULL);
/*!40000 ALTER TABLE `Person` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2019-01-23 15:51:56
